library(dplyr)
library(stringi)

data_initialize <- function() {
  ######################################################################################
  # Initialize data
  #
  # Args:
  #   
  # Returns:
  #   
  #######################################################################################
 
  dataFilePrefix <- "COPD3_"
 
  # used for some filter searches where dont pay attention to specific site
  assign("IgnoreSiteFilter", FALSE, envir=.GlobalEnv)
  
  # used to access last query result from R session tab
  assign("Last_query_result", " ", envir=.GlobalEnv)
 
  p <- progress_estimated(5)
  print(p$tick())
  print("Reading data...")
  
  read_data(dataFilePrefix)
  
  print(p$tick())
  print("Data cleanup...")
  
  cleanup_data()
  
  print(p$tick())
  print("Selection setup")
  
  setup_selection_choices()
  
  print(p$tick())
  print("Combining data...")
  
  combine_data_info()
  
  print(p$tick())
 
}

read_data <- function(dataFilePrefix) {
######################################################################################
# Reads in data forcing no factoring. Make sure data file is tab delimited 
# since commas exist in some text fields
#
#
# Args: dataFilePrefix  - data file prefix
#   
# Returns:
#   
#######################################################################################

  # set below for smaller input files for debug
  shortFile <- "_10000"
  #shortFile <- ""
  
  testResultsDataFiles <- c(paste(dataFilePrefix,"lb",shortFile,sep=""),
                            paste(dataFilePrefix,"eg",shortFile,sep=""),paste(dataFilePrefix,"vs",shortFile,sep=""))
  
  patientDataFile <- paste(dataFilePrefix,"patients",sep="")
 
  protocolStartsDataFile <- paste(dataFilePrefix,"protocol_starts",sep="")
  
  # check for files in rds format, if not then create from .txt tab delimited file
  for (ifile in testResultsDataFiles)
  {
      if (!file.exists(paste(ifile,".rds",sep="")))
      {
         test_results <- read.delim( paste(ifile,".txt",sep=""), fileEncoding="UTF-8-BOM",colClasses="character")    
         saveRDS(test_results,file=paste(ifile,".rds",sep=""))
         
         # save random sample for smaller files for debugging
         test_results <- sample_n(test_results,10000)
         saveRDS(test_results,file=paste(ifile,"_10000",".rds",sep=""))
      }
  }
  if (!file.exists(paste(patientDataFile,".rds",sep="")))
  {
    patient_data <- read.delim( paste(patientDataFile,".txt",sep=""), fileEncoding="UTF-8-BOM",colClasses="character")    
    saveRDS(patient_data,file=paste(patientDataFile,".rds",sep=""))
  }
  
  if (!file.exists(paste(protocolStartsDataFile,".rds",sep="")))
  {
    protocol_starts_data <- read.delim( paste(protocolStartsDataFile,".txt",sep=""), fileEncoding="UTF-8-BOM",colClasses="character")    
    saveRDS(protocol_starts_data,file=paste(protocolStartsDataFile,".rds",sep=""))
  }
 
  # see how long it takes to read in data
  start.time <- Sys.time()
  
  test_results <- {}
  for (ifile in testResultsDataFiles)
  {
    in_data <- readRDS(paste(ifile,".rds",sep=""))
  
    # rename columns
    colnames(in_data)[which(colnames(in_data)==paste(in_data$DOMAIN[1],'TEST',sep=""))] <- "TEST"
    colnames(in_data)[which(colnames(in_data)==paste(in_data$DOMAIN[1],'TESTCD',sep=""))] <- "TESTCD"
    colnames(in_data)[which(colnames(in_data)==paste(in_data$DOMAIN[1],'STRESU',sep=""))] <- "STRESU"
    colnames(in_data)[which(colnames(in_data)==paste(in_data$DOMAIN[1],'STRESN',sep=""))] <- "STRESN"
    colnames(in_data)[which(colnames(in_data)==paste(in_data$DOMAIN[1],'DTC',sep=""))] <- "DTC"
    
    test_results <- rbind(test_results, in_data)
  }
  
  assign( "Test_results_data", test_results, envir = .GlobalEnv )
  
  assign("Patient_data", readRDS(paste(patientDataFile,".rds",sep="")),envir = .GlobalEnv)
 
  assign("Protocol_starts_data", readRDS(paste(protocolStartsDataFile,".rds",sep="")),envir = .GlobalEnv)
       
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
     
  # for randomly taking subset of the data for plotting
  assign("SampleSize", 100000,envir = .GlobalEnv)
       
  if (SampleSize > nrow(Test_results_data))
     SampleSize <<- nrow(Test_results_data)
  #Test_results_data <<- sample_n(Test_results_data,SampleSize)
 
}

cleanup_data <- function() {
#######################################################################################
# Cleans up data 
#
# Args:
#   
# Returns:
#   
#######################################################################################
  
  # get rid of NA's
  idx <- which(is.na(Test_results_data$STRESN))
  if ( length(idx) != 0 )
    Test_results_data <<- Test_results_data[-idx,]
  
  # get rid of non numerics in lab test results (not looking at ones with '<' and '>' )
  idx <- which(suppressWarnings(is.na(as.numeric(as.character(Test_results_data$STRESN)))))
  Test_results_data <<- Test_results_data[-idx,]
  
  # convert strings to numeric (assuming no factors!!)
   Test_results_data$STRESN <<- as.numeric(Test_results_data$STRESN)
#   Test_results_data$SUBJID <<- as.numeric(Test_results_data$SUBJID)
   
   # make sure proper date format
   Test_results_data$DTC <<- as.Date(Test_results_data$DTC)
   
   # trim up long SITE names
   Test_results_data$SITE <<- substr(Test_results_data$SITE,1,7)
   Patient_data$SITE <<- substr(Patient_data$SITE,1,7)
   
   # create long test name out of domain and test
   Test_results_data$testnamelong <<- paste(Test_results_data$DOMAIN,".",Test_results_data$TESTCD,sep="")

   # fix dates
   Patient_data$BRTHDTC <<- as.Date(Patient_data$BRTHDTC)
   Test_results_data$BRTHDTC <<- as.Date(Test_results_data$BRTHDTC)
 
}

setup_selection_choices <- function() {
#######################################################################################
# Sets up choices for selection in selectInput in ui.R
#
# Args:
#   
# Returns:
#   
#######################################################################################
  
  assign( "StudyTitle", Test_results_data$STUDYID[1], envir = .GlobalEnv )
  
  assign("InvestigatorNames", select(Test_results_data,SITE) %>% distinct(SITE) %>% arrange(SITE), envir=.GlobalEnv)
  assign("CountryNames", select(Patient_data,COUNTRY) %>% distinct(COUNTRY) %>% arrange(COUNTRY),envir=.GlobalEnv)
  assign("TestNames", select(Test_results_data,testnamelong) %>% distinct(testnamelong) %>% arrange(testnamelong), envir = .GlobalEnv)
  assign("ProtocolIds", select(Protocol_starts_data,STUDYID) %>% distinct(STUDYID) %>% arrange(STUDYID), envir = .GlobalEnv)
 
  }


combine_data_info <- function() {
#######################################################################################
# Combine data info needed for future queries
#
# Args:
#   
# Returns:
#
# Note use <<- for global variables
#   
#######################################################################################
 
 
   # get overall means and sd for site,country and protocol for different data displays (not that useful-need to do by test)
  by_site <- group_by(Test_results_data,SITE)
  siteMean <- summarize(by_site,msiteval=mean(STRESN),sdsiteval=sd(STRESN))
  
  by_protocol <- group_by(Test_results_data,STUDYID)
  protocolMean <- summarize(by_protocol,mprotocolval=mean(STRESN),sdprotocolval=sd(STRESN))
  
  by_country <- group_by(Test_results_data,COUNTRY)
  countryMean <- summarize(by_country,mcountryval=mean(STRESN),sdcountryval=sd(STRESN))
  
  # put everything in this table for easier access
  Test_results_data <<- inner_join(Test_results_data,siteMean,by="SITE")
  Test_results_data <<- inner_join(Test_results_data,protocolMean,by="STUDYID")
  Test_results_data <<- inner_join(Test_results_data,countryMean,by="COUNTRY")
  
  # test information
  assign( "Test_info_data", unique(select(Test_results_data,testnamelong,STRESU)), envir = .GlobalEnv )

}

add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title") {
#######################################################################################
# Add title to ggvis plot - needed to be done in round about way
#
# Args:  vis, ...,
#        x_lab - for setting x-axis label
#        title - plot title
#
#   
# Returns:
#
########################################################################################

  add_axis(vis, "x", title = x_lab) %>% 
    add_axis("x", orient = "top", ticks = 0, title = title,
             properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 3)
             ), ...)
}

get_patient_age <- function(x) {
  #######################################################################################
  # Get patient age at trial start date
  #
  # Args:  x  data 
  #
  #   
  # Returns: Int: Age in years
  #
  ########################################################################################
  patientAge <- round((as.Date(x$startedon) - as.Date(x$BRTHDTC))/365)
  return(patientAge)
}

# call here 
data_initialize()

#######################################################################################
# Queries used.  They take too long to run interactively 
# so save to tab delimited file then convert to rds
#######################################################################################


##########################################################################

