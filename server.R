library(shiny)
library(dplyr)
#library(rsconnect)

DEBUG <- FALSE

SCREENWIDTH <- 1000
SCREENHEIGHT <- 800

shinyServer(function(input, output, session) {

  # if u want to see what tab you're in 
  #output$text <- renderText({paste0("Viewing tab \"", input$tabs, "\"")})
  
  ntext <- eventReactive(input$submitButton, {
    input$iRstatement
    
  })
  
  output$nText <- renderUI({
    save.image()
    rstatement <- paste0("load('.RData'); ",ntext())
    write(rstatement,file="tmp.txt")
    tstr <- system("Rscript tmp.txt",intern=TRUE,invisible=FALSE)
   
    for (i in 1:length(tstr))
    {
      tstr[i] <- paste0(tstr[i],"<br/>")
    }
    
    HTML(tstr)
    
  })
  
  ###########################
  # tooltip for test results 
  ###########################
  test_tooltip <- function(x) {

    if(is.null(x)) return(NULL)
    if (is.null(x$ID))return(NULL)
 
    all_points <- isolate(filter_testResults())
    thePoint <- all_points[all_points$ID == x$ID, ]
    
    if (is.null(thePoint) || !nrow(thePoint)) return(NULL)
   
    patientAge <- get_patient_age(thePoint)
    if (thePoint$ARMCD=="SCRNFAIL")
      screen <- "Failed"
    else
      screen <- "Passed"
    
    paste0("<b>","Test: ","</b>", thePoint$testnamelong,"<br>",
           "<b>","Protocol ID: ","</b>",thePoint$STUDYID.x, "<br>",
           "<b>","Trial Start: ","</b>",thePoint$startedon,"<br>",
           "<b>","Patient ID: ","</b>",thePoint$SUBJID, "<br>",
           "<b>","Visit Test ID: ","</b>",thePoint$visittestid, "<br>",
           "<b>","Drug: ","</b>",thePoint$drugname, "<br>",
           "<b>","SITE: ","</b>",thePoint$SITE, "<br>",
           "<b>","Country: ","</b>",thePoint$COUNTRY, "<br>",
           "<b>","Gender: ","</b>",thePoint$SEX,"<br>",
           "<b>","Age at Trial: ","</b>",patientAge, "<br>",
           "<b>","DOB: ","</b>",thePoint$BRTHDTC, "<br>",
           "<b>","Screen: ", "</b>",screen
           )
  }
  ###########################
  # tooltip for sfr
  ###########################
  sfr_tooltip <- function(x) {
    
    if (is.null(x)) return(NULL)
    if (is.null(x$ID))  return(NULL)
   
    # Pick out the one with this ID
    all_points <- isolate(filter_sfr())
    thePoint <- all_points[all_points$ID == x$ID, ]
    
    if (is.null(thePoint) || !nrow(thePoint)) return(NULL)

    toolTipStr <- paste0(  "<b>","Country: ", "</b>",thePoint$COUNTRY,"<br>",
                           "<b>","SFR: ","</b>",format(thePoint$sfr,digits=3), "<br>",
                           "<b>","Protocol: ","</b>", thePoint$STUDYID,"<br>",
                           "<b>","Total Patients: ","</b>",thePoint$totalPatients, "<br>",
                           "<b>","Total Failed: ","</b>",thePoint$totalScreenFailed, "<br>"
    )
    if (input$iby=="site")
    {
       paste0( "<b>","Site: ", thePoint$SITE,"</b><br>",toolTipStr)
    }
    else #country
    {
      toolTipStr
    }
  }
  ###########################
  # tooltip for histogram
  ###########################
  histo_tooltip <- function(x) {
 
    if (is.null(x)) return(NULL)
 
    if (length( grep("SITE",names(x)))>0)
    {
      paste0("<b>SITE:</b>",x$SITE,"</b>","<br>",
             "<b>Patient:</b>",x$x_,"<br>",
             "<b>",x$stack_upr_ - x$stack_lwr_,"</br>")
    }
    else
    {
      str <- paste0(names(x), ": ", format(x), collapse = "<br />")
      paste0(str, "<br><b>",x$stack_upr_ - x$stack_lwr_, "</b>")
    }
  }
  
   ###################################
  # filter for test results
  ###################################
  filter_testResults <- reactive(
  {
    if (DEBUG)
      browser()
 
    # need to define these local vars due to bug in filter that doesn't work with input$
    itname <- input$itname
    ipiname <- input$ipiname
    mintVal <- input$tVal[1]
    maxtVal <- input$tVal[2]
   
    iprotocol <- input$iprotocol
    icountry <- input$icountry
    igender <- input$igender
    minAge <- input$iage[1]
    maxAge <- input$iage[2]
    
    if (is.null(itname))
      itname <- t(TestNames)
    if (is.null(ipiname) || IgnoreSiteFilter)
      ipiname <- t(InvestigatorNames)
    if (is.null(iprotocol))
      iprotocol <- ProtocolIds
   
    if (is.null(icountry))
      icountry <- t(CountryNames)
    if (igender=="Both")
      igender <- c("M","F")
  
    # filter for either all,screen passed or screen failed patients
    switch( input$ipatienttype,
           all = {patients <- distinct(select(Test_results_data,SUBJID))},
           screenpassed = {patients <- select(filter(Patient_data,ARMCD!="SCRNFAIL"),SUBJID)},
           screenfailed = {patients <- select(filter(Patient_data,ARMCD=="SCRNFAIL"),SUBJID)}
    )
    #patients <- as.numeric(patients$SUBJID)
    visitData <- filter(Test_results_data,(SUBJID %in% patients$SUBJID))
    
    # display results depending on normalization selection
    switch( input$idisplayresults,
            actualDataValue = {
              visitData$displayValue = visitData$STRESN
            },
            siteMeanDataValue = {
              visitData$displayValue <- (visitData$STRESN - visitData$msiteval) / visitData$sdsiteval
            },
            protocolMeanDataValue = {
              visitData$displayValue <- (visitData$STRESN - visitData$mprotocolval) / visitData$sdprotocolval
            },
            countryMeanDataValue = {
              visitData$displayValue <- (visitData$STRESN - visitData$mcountryval) / visitData$sdcountryval
            }
            
    )
    # Apply filters
    m <- visitData %>%
      filter(
        (testnamelong %in% itname)
        ,STRESN > mintVal
        ,STRESN <= maxtVal
        ,(SITE %in% ipiname)
        ,(STUDYID %in% iprotocol)
        ,(COUNTRY %in% icountry)
        ,(SEX %in% igender)
        
      ) %>%
      arrange(DTC)
    
    m <- as.data.frame(m)
   
    # add protocol start date so it can be used to find age of patient during trial
    m <- inner_join(m,Protocol_starts_data,by="STUDYID")

    patient_age <- get_patient_age(m)
    
    m <- filter(m, patient_age>=minAge,patient_age<=maxAge)
   
    # set the ID for tooltip info
    if (nrow(m)!=0)
       m$ID <- 1:nrow(m)
    else
    {
      # not sure how to add column to 0 length df so here's my stab
      m[1,1] <- 0
      m$ID <- 1
      m <- filter(m,ID==-1)
    }
    m
 })
  #########################################
  # filter for screen failure rate 
  #########################################
  filter_sfr <- reactive(
    {
      itname <- input$itname
      ipiname <- input$ipiname
      iprotocol <- input$iprotocol
      icountry <- input$icountry
      igender <- input$igender
      
      if (is.null(itname))
        itname <- t(TestNames)
      if (is.null(ipiname))
        ipiname <- t(InvestigatorNames)
      if (is.null(iprotocol))
        iprotocol <- t(ProtocolIds)
      if (is.null(icountry))
        icountry <- t(CountryNames)
      if (igender=="Both")
        igender <- c("M","F")
      
      # Apply filters
      # if specific tests were selected then use info from test_results_data
      if (!is.null(input$itname))
      {
        patients <- Test_results_data %>% filter((testnamelong %in% itname) )
      }
      else 
        patients <- Test_results_data
      
      patients <- distinct(select(patients,SUBJID,SITE,COUNTRY,SEX,ARMCD,STUDYID,SITEID))
      
      m <- patients %>%
        filter(
          (SITE %in% ipiname)
          ,(COUNTRY %in% icountry)
          ,(SEX %in% igender)
          ,(STUDYID %in% iprotocol)
        ) %>%
        arrange(SUBJID)
      
      m <- as.data.frame(m)
      
      patients$failed <- patients$ARMCD=="SCRNFAIL"
      
      if (input$iby=="site" || input$isummary=="site")
      {
        # SFR by site
        
        # group by site
        by_site <- group_by(patients,SITE,COUNTRY)
        screenFailureRateSite <- summarise( by_site
                                            ,totalPatients = n()
                                            ,totalScreenFailed = sum(failed)
                                            ,sfr = totalScreenFailed / totalPatients)
        
        # set the ID for tooltip info                                    
        if (nrow(screenFailureRateSite))
          screenFailureRateSite$ID <- as.numeric(1:nrow(screenFailureRateSite))
        else
        {
          # not sure how to add column to 0 length df so here's my stab
          screenFailureRateSite[1,] <- 1
          screenFailureRateSite$ID <- 1
          screenFailureRateSite <- filter(screenFailureRateSite,ID==-1)
        }
        
        m <- as.data.frame(screenFailureRateSite)
        
      }
      else  
      {
        # SFR by country
        by_country <- group_by(patients,COUNTRY)
        
        screenFailureRateCountry <- summarise(by_country,
                                              totalPatients = n(),
                                              totalScreenFailed = sum(failed),
                                              sfr = totalScreenFailed / totalPatients
                                              
        )
        if (nrow(screenFailureRateCountry))
          screenFailureRateCountry$ID <- 1:nrow(screenFailureRateCountry)
        else
        {
          # not sure how to add column to 0 length df so here's my stab
          screenFailureRateCountry[1,] <- 0
          screenFailureRateCountry$ID <- 1
          screenFailureRateCountry <- filter(screenFailureRateCountry,ID==-1)
        }
        
        m <- as.data.frame(screenFailureRateCountry)
      }
      m
    })
  
  
  ########################################
  # Plot for test values
  ########################################
  vis <- reactive ({
   
    if (DEBUG)
       browser()
   
    # get filtered data based on user selection
    m <- filter_testResults()
  
    #if looking at a specific test then use all data points assuming they won't be enough to choke ggvis
   
    if (!is.null(input$itname) || SampleSize >= nrow(m))
      testResults <- m
    else
    {
      # can't plot all points because R crashes so take sample
      testResults <- sample_n(m,SampleSize) 
    }
   
    # Title the plot and y-axis based on test name and protocol
 
    if (!is.null(input$itname) && length(input$itname)==1)
    {
      plotTitle <- input$itname
      y_label <- filter(Test_info_data,grepl(input$itname[1],testnamelong,fixed=TRUE))$STRESU[1]
      if (is.na(y_label))
        y_label <- "Test Value"
    }
    else  # multiple tests selected so put generic
    {
      plotTitle <- "Lab Results"
      y_label <- "Test Value"
    }
  
    plotTitle <- "Protocol:"  
    
    if (!is.null(input$iprotocol[1]))
       plotTitle <- paste(plotTitle,"Protocol:",input$iprotocol[1])
    
    # Plot it
    # Note:bug in ggvis doesn't display correctly with only 1 point
   
    # set this for R session tab
    Last_query_results <<- testResults
    
#     testResults %>%
#         ggvis(~ID, ~displayValue) %>%
#         layer_points(size := 100, size.hover := 200,fillOpacity := 0.6, fillOpacity.hover := 1,fill=~COUNTRY) %>%
#         add_title(title = plotTitle, x_lab = "Visit") %>%
#         add_axis("y", title = y_label) %>%
#         add_tooltip(test_tooltip, "hover") %>%
#         set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
    
    tmpvis <- ggvis(testResults,~ID, ~displayValue) 
    tmpvis <- layer_points(tmpvis,size := 100, size.hover := 200,fillOpacity := 0.6, fillOpacity.hover := 1,fill=~COUNTRY) 
    tmpvis <- add_title(tmpvis,title = plotTitle, x_lab = "Visit")
    tmpvis <-  add_axis(tmpvis,"y", title = y_label) 
    tmpvis <-  add_tooltip(tmpvis,test_tooltip, "hover") 
    set_options(tmpvis,width = SCREENWIDTH, height = SCREENHEIGHT)
  
    
  }) # vis
  
  
  
  
  bind_shiny(vis,"plotLabResults")

  #########################################
  # Plot for screen failure rate by site
  #########################################
  vis2 <- reactive({
     
     if (DEBUG)
        browser()   
     
     iprotocol <- input$iprotocol[1]
     if (is.null(iprotocol))
       iprotocol <- " "
     
     # filter data
     m <- filter_sfr()
     
     # set this for R session tab
     Last_query_results <<- m
     
     # plot it
     if (input$iby=="site" && nrow(m))
     {
        m  %>%
          ggvis(~ID,~sfr) %>%
          layer_points(size := ~totalPatients*10, size.hover := 200,fillOpacity := 0.6, fillOpacity.hover := 1,fill=~COUNTRY) %>%
          add_title(title = paste("Screen Failure Rate By Site   (point size indicates # of patients)  Protocol:",iprotocol), x_lab = "Sites") %>%
          add_axis("y", title = "Screen Failure Rate") %>%
          add_tooltip(sfr_tooltip, "hover") %>%
          set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
     }
     else if (input$iby=="country" && nrow(m))
     {
        m  %>%
         ggvis(~ID,~sfr) %>%
         layer_text(x=~ID+0.05, y=~sfr, text:=~COUNTRY,baseline:="middle", font:="Arial",fontSize:=10) %>%
         layer_points(size:=~totalPatients*5, size.hover := 200,fillOpacity := 0.6, fillOpacity.hover := 1,fill=~COUNTRY) %>%
         add_title(title = paste("Screen Failure Rate By Country  (point size indicates # of patients) Protocol:",iprotocol), x_lab = "Countries") %>%
         add_axis("y", title = "Screen Failure Rate") %>%
         add_tooltip(sfr_tooltip, "hover") %>%
         hide_legend('fill') %>%
         set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
     }
     else  # no rows returned
     {
         m <- Test_results_data[1,]  # set to fake
         m$ID <- 1
         m  %>%
           ggvis(~ID,~STRESN) %>%
           layer_points() %>%
           add_title(title = paste("No values found:",iprotocol[1]), x_lab = " ") %>%
           set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
     } 
     
   
  }) #vis2
  bind_shiny(vis2,"plotSFR")
   
  ###############################################################
  # boxplots
  ###############################################################
  vis3 <- reactive({
     
     if (DEBUG)
       browser()
     
     iprotocol <- input$iprotocol[1]
     if (is.null(iprotocol))
       iprotocol <- " "
   
     m <- filter_testResults()
     
     # set this for R session tab
     Last_query_results <<- m
    
     if (nrow(m)==0)  # layer_boxplots() doesn't work with 0 row
     {
       m <- Test_results_data[1,]  # set to fake
       m  %>%
         ggvis(~COUNTRY,~STRESN) %>%
         layer_boxplots() %>%
         add_title(title ="No values found") %>%
         set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
     }
     else if (input$iby=="country")
     {
       m  %>%
         ggvis(~COUNTRY,~STRESN) %>%
         layer_boxplots(stroke=~COUNTRY) %>%
         #add_title(title = paste("Box plot of test values by COUNTRY:",input$iprotocol[1]), x_lab = "Countries") %>%
         #add_title(title = paste("Box plot of test values by COUNTRY:",input$iprotocol[1])) %>%
         add_axis("x", title=" ", properties = axis_props(labels = list(angle = 90, align = "left", fontSize = 10))) %>%
         set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
     }
     else   # by site
     {
       m  %>%
         ggvis(~SITE,~STRESN) %>%
         layer_boxplots(stroke=~SITE) %>%
         #add_title(title = paste("Box plot of test values by site:",input$iprotocol[1]), x_lab = "Sites") %>%
         #add_title(title = paste("Box plot of test values by site:",input$iprotocol[1])) %>%
         add_axis("x", title=" ", properties = axis_props(labels = list(angle = 90, align = "left", fontSize = 10))) %>%
         set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
     }
  }) #vis3
   
  bind_shiny(vis3,"plotBoxPlot")
  
  #######################
  # histogram 
  #######################
  vis4 <- reactive({
     
    if (DEBUG)
       browser()
    
    if (input$ihistogram == "protocoldeviation")
    {
      m <- Protocol_deviation_data
      by_site <- group_by(m,siteid)
      m <- summarise(by_site,cnt=n())
      m$id <- 1:nrow(m)
      
      # set this for R session tab
      Last_query_results <<- m
      
      m %>%
        ggvis( x=~siteid, y=~cnt ) %>%
        layer_bars(fill:="#E74C3C") %>%
        #add_tooltip(function(df) {(df$stack_upr_ - df$stack_lwr_)}) %>%
        add_tooltip(histo_tooltip,"hover") %>%
        #add_title(title = "Protocol Deviations", x_lab = "Sites") %>%
        add_axis("x", title=" ", properties = axis_props(labels = list(angle = 90, align = "left", fontSize = 10))) %>%
        add_axis("y", title = "Counts") %>%
        set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
      
    }
    else
    {
       switch(input$ipatienttype,
             all = { patients <- Patient_data },
             screenpassed = { patients <- filter(Patient_data,ARMCD!="SCRNFAIL")},
             screenfailed = { patients <- filter(Patient_data,ARMCD=="SCRNFAIL")} )
      
       switch( input$ihistogram,
              test  = {  m <- filter_testResults() },
                 age = { m <- patients  
                         # this is a kluge sorry
                         m$startedon <- rep(Protocol_starts_data$startedon[1],nrow(patients))
                         m$age <- as.numeric(get_patient_age(m))
                       },
              gender = { m <- patients
                         m$ngender <- m$SEX
                         m$ngender[m$ngender=="F"] <- 1
                         m$ngender[m$ngender=="M"] <- 2
                         m$ngender <- as.numeric(m$ngender)
                      }
          )
      
          xLab <- input$ihistogram
    
          # nothing to display
          if (nrow(m)==0)  # layer_histograms() doesn't work with 0 row
          {
              m <- Test_results_data[1,]  # set to fake
              m  %>%
                  ggvis(~STRESN) %>%
                    layer_histograms(width=5,center=35,fill:="#E74C3C") %>%
                    add_tooltip(histo_tooltip,"hover") %>%
                    add_title(title = paste("No values found:",input$iprotocol[1]), x_lab = "Test Values") %>%
                    add_axis("y", title = "Count")  %>%
                    set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
          }
          else 
          {
              # set this for R session tab
              Last_query_results <<- na.omit(m)
       
              na.omit(m)  %>%
                  ggvis( switch(input$ihistogram,
                              test  = {~STRESN},
                                age = {~age},
                             gender = {~ngender}
                          )) %>%
                  layer_histograms(fill:="#E74C3C") %>%
                  #add_tooltip(function(df) (df$stack_upr_ - df$stack_lwr_)) %>%
                  add_tooltip(histo_tooltip,"hover") %>%
                  add_title(title = paste("Protocol:",input$iprotocol[1]), x_lab = xLab) %>%
                  add_axis("y", title = "Count")  %>%
                  set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
          } 
       }
  }) #vis4
  bind_shiny(vis4,"plotHistogram")
   
  ##################################
  # Outliers
  ##################################
  vis5 <- reactive({
     
     if (DEBUG)
       browser()
     
     m <- filter_testResults()
   
     switch (input$ioutliertype,
           test    = {
                       outliers <- m %>% group_by(SITE,testnamelong) %>% compute_boxplot(~STRESN)
                       colnames(outliers)[which(colnames(outliers)=='testnamelong')] <- "otype"
                     },
       sitepertest =  {
                       outliers <- m %>% group_by(SITE,testnamelong) %>% compute_boxplot(~STRESN)
                       colnames(outliers)[which(colnames(outliers)=='SITE')] <- "otype"
                      },
       sitetotal  = {
                        outliers <- m %>% group_by(SITE,testnamelong) %>% compute_boxplot(~STRESN)
                        colnames(outliers)[which(colnames(outliers)=='SITE')] <- "otype"
                        outliers$outlier_counts<-sapply(outliers$outliers_, function(x){length(x)})
                    
                   },
         country = {
                        outliers <- m %>% group_by(COUNTRY,testnamelong) %>% compute_boxplot(~STRESN)
                        colnames(outliers)[which(colnames(outliers)=='COUNTRY')] <- "otype"
                        outliers$outlier_counts<-sapply(outliers$outliers_, function(x){length(x)})
                  },
         patient = {
                        outliers <- m %>% group_by(SUBJID,testnamelong,SITE) %>% compute_boxplot(~STRESN)
                        colnames(outliers)[which(colnames(outliers)=='SUBJID')] <- "otype"
                        outliers$outlier_counts<-sapply(outliers$outliers_, function(x){length(x)})
                    }
      ) #switch
     
     if (nrow(m))
     {
       # get outlier counts for each test
       outliers$outlier_counts<-sapply(outliers$outliers_, function(x){length(x)})
       
       # group accordingly
       
       # divide by number of patients if it was checked
       if (input$ipatientnorm)
       {
         
         if (input$ioutliertype == "sitetotal")
         {
           # add up all tests into one total
           outliers <- group_by(outliers,otype) %>% summarise(newoutlier_counts=sum(outlier_counts))
           outliers$outlier_counts <- outliers$newoutlier_counts
         }
         if (input$ioutliertype == "sitetotal" || input$ioutliertype == "sitepertest")
         {
           # group by site to get number of patients per site
           by_site <- group_by(Patient_data,SITEID)
           numPatientsPerSite <- summarise(by_site, numPatients=n())
           tmp <- inner_join(outliers,numPatientsPerSite,by=c("otype" = "SITEID"))
           tmp$outlier_counts <- tmp$outlier_counts / tmp$numPatients
           outliers <- tmp
         }
         else if (input$ioutliertype == "country")
         {
           # group by country to get number of patients per country
           by_country <- group_by(outliers,otype)
           numPatientsPerCountry <- summarise(by_country, numPatients=n())
           tmp <- inner_join(outliers,numPatientsPerCountry,by="otype")
           tmp$outlier_counts <- tmp$outlier_counts / tmp$numPatients
           outliers <- tmp
         }
         
       }
     } # nrow(m)
     
     # only show sites with at least this many outliers
     inumoutliers <- input$inumoutliers
     if (inumoutliers=='')
       inumoutliers <- 0
     else
       inumoutliers <- as.numeric(inumoutliers)
     
      # get only those over a user specified amount   
      outliers <-outliers[outliers$outlier_counts>inumoutliers,]
      
      # dummy df so ggvis doesn't choke
      if (nrow(outliers)==0)
      {
         outliers <- c("None Found",1)
         outliers <- t(outliers)
         outliers <- data.frame(outliers)
         colnames(outliers)[1] <- "otype"
         colnames(outliers)[2] <- "outlier_counts"
         outliers$outlier_counts <- as.numeric(outliers$outlier_counts)
      }
      
      # set this for R session tab
      Last_query_results <<- outliers
      
      if (input$ioutliertype == "patient")
      {
        #use site as fill
        
         outliers  %>%
           ggvis( x=~otype, y=~outlier_counts ) %>%
           layer_bars(fill=~SITE) %>%
           add_tooltip(histo_tooltip,"hover") %>%
           add_axis("x", title=" ", properties = axis_props(labels = list(angle = 90, align = "left", fontSize = 10))) %>%
           set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
      }
      else
      {
         outliers  %>%
            ggvis( x=~otype, y=~outlier_counts ) %>%
            layer_bars(fill:="#E74C3C") %>%
            add_tooltip(histo_tooltip,"hover") %>%
            add_axis("x", title=" ", properties = axis_props(labels = list(angle = 90, align = "left", fontSize = 10))) %>%
            set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
      }
  
  }) #vis5
  
  bind_shiny(vis5,"plotOutliers")

  #######################
  # densities
  #######################
  vis6 <- reactive({
     
     if (DEBUG)
       browser()
     
     switch(input$ipatienttype,
            all = { patients <- Patient_data },
            screenpassed = { patients <- filter(Patient_data,ARMCD!="SCRNFAIL")},
            screenfailed = { patients <- filter(Patient_data,ARMCD=="SCRNFAIL")} )
     
      switch( input$idensity,
               test  = { 
                         IgnoreSiteFilter <<- TRUE
                         m <- filter_testResults()
                         IgnoreSiteFilter <<- FALSE
               },
               age = { m <- patients
                               # this is a kluge sorry
                       m$startedon <- rep(Protocol_starts_data$startedon[1],nrow(Patient_data))
                       m$age <- as.numeric(get_patient_age(m))
               },
               gender = { m <- patients
                          m$ngender <- m$SEX
                          m$ngender[m$ngender=="F"] <- 1
                          m$ngender[m$ngender=="M"] <- 2
                          m$ngender <- as.numeric(m$ngender)
               }
       )
       if (nrow(m)==0)  # doesn't work with 0 row
       {
         m <- Test_results_data[1,]  # set to fake
         m  %>%
           ggvis(~STRESN) %>%
           layer_histograms(width=5,center=35,fill:="#E74C3C") %>%
           add_title(title = paste("No values found:",input$iprotocol[1]), x_lab = "Test Values") %>%
           add_axis("y", title = "Count")  %>%
           set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
       }
       else 
       {
          # add column to group by
          m$sitegroup <- rep("All Sites",nrow(m))
          
          # now add just for site
          if (length(input$ipiname)!= 0)
          {
            ipiname <- input$ipiname[1]
            m2 <- filter(m,grepl(ipiname,SITE))
            m2$sitegroup <- rep("This Site",nrow(m2))
            mboth <- rbind(m,m2)
          }
          else
          {
            mboth <- m
          }
          
          # set this for R session tab
          Last_query_results <<- na.omit(mboth)
          
          na.omit(mboth) %>% group_by(sitegroup) %>%
             ggvis( switch(input$idensity,
                       test  = {~STRESN},
                       age = {~age},
                       gender = {~ngender}, fill=~factor(sitegroup)
             )) %>%
             layer_densities( adjust = input$ibandwidth, kernel=input$ikernel,strokeWidth := 2,stroke := "black",fill=~factor(sitegroup),fillOpacity := 0.6 ) %>%
             set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
       } 
     
  }) #vis6
  bind_shiny(vis6,"plotDensities")
   
  #############
  # CDF
  #############
  vis7 <- reactive({ 
     
     if (DEBUG)
       browser()
     
     # patients - ALL, Screen Passed, Screen Failed
     switch(input$ipatienttype,
            all = { patients <- Patient_data },
            screenpassed = { patients <- filter(Patient_data,ARMCD!="SCRNFAIL")},
            screenfailed = { patients <- filter(Patient_data,ARMCD=="SCRNFAIL")} )
     
   
     # cdf for test result or patient age(just to make sure cdf is correctly working)
     switch( input$icdf,
             test  = {  
                        IgnoreSiteFilter <<- TRUE
                        m <- filter_testResults()
                        IgnoreSiteFilter <<- FALSE
                        
                        m$sitegroup <- rep("All Sites",nrow(m))
                        
                        if (nrow(m))
                        {
                          # qq future:if individual sites or countries were selected then show different cdf plots
                          yvals <- ecdf(m$STRESN)(m$STRESN)
                          #xvals <- seq(min(m$STRESN,na.rm=TRUE),max(m$STRESN,na.rm=TRUE),1)
                          xvals <- m$STRESN
                          cdf <- data.frame(xvals,yvals)
                          cdf$sitegroup <- rep("All Sites", nrow(cdf))
                          mall <- cdf
                          
                          if (length(input$ipiname)!=0)
                          {
                             m <- filter(m,grepl(input$ipiname[1],SITE))
                             yvals <- ecdf(m$STRESN)(m$STRESN)
                             #xvals <- seq(min(m$STRESN,na.rm=TRUE),max(m$STRESN,na.rm=TRUE),1)
                             xvals <- m$STRESN
                             cdfsite <- data.frame(xvals,yvals)
                             cdfsite$sitegroup <- rep("This Site",nrow(cdfsite))
                             mall <- rbind(cdf,cdfsite)
                          }
                        }
                        else # make something for no rows returned
                        {
                          xvals <- 1
                          yvals <- 1
                          mall <- data.frame(xvals,yvals)
                          mall$sitegroup <- "All Sites"
                        }
                        
             },
             age = { m <- patients
                        # this is a kluge sorry
                     m$startedon <- rep(Protocol_starts_data$startedon[1],nrow(patients))
                     m$age <- as.numeric(get_patient_age(m))
                     cdf <- ecdf(m$age)
                     xvals <- seq(min(m$age,na.rm=TRUE),max(m$age,na.rm=TRUE),1)
                     yvals <- cdf(xvals)
                     yvals <- data.frame(yvals)
                     mall <- cbind(xvals,yvals)
                     mall$sitegroup <- rep("All Sites",nrow(mall))
             }
            
     )
    
    #if looking at a specific test then use all data points assuming they won't be enough to choke ggvis
    
    if (!is.null(input$itname) || SampleSize >= nrow(mall))
      pointsToPlot <- mall
    else
    {
      # can't plot all points because R crashes so take sample
      pointsToPlot <- sample_n(mall,SampleSize) 
    }
     
    # set this for R session tab
    Last_query_results<<- pointsToPlot
     
    pointsToPlot %>%
                 ggvis(x=~xvals,y=~yvals,fill=~factor(sitegroup)) %>%
                 layer_points(stroke=~factor(sitegroup)) %>%
                 add_axis("y", title = "Cumulative Value")  %>%
                 set_options(width = SCREENWIDTH, height = SCREENHEIGHT)
     
  }) #vis7
  bind_shiny(vis7,"plotCDF")
   
  ############################################################################################################################
  ##############################
  # output variable for the UI
  ##############################
  
  # to display the number of rows returned for test results
  output$n_tests <- renderText({ nrow(filter_testResults()) }) 
   
  ################
  # summary info
  ################
   
  output$summary <- renderDataTable({
    
     switch( input$isummary,
           test = {
             by_tests <- group_by(Test_results_data,testnamelong)
             testCounts <- summarise(by_tests, count = n() )
             testCounts <- arrange(testCounts,desc(count))
             
             #add other info about test
             m <- inner_join(testCounts,select(Test_info_data,STRESU,testnamelong=testnamelong),by="testnamelong")
             m <- distinct(m)
             m
           },
           site = {
             # summary sfr by site
             m <- isolate(filter_sfr())
             m$STUDYID <- NULL
             m <- distinct(m)
             m
           },
           patient = {
             # summary patient info
             Patient_data         
           },
           country = {
             # summary sfr by country 
             allCountriesSFR <- isolate(filter_sfr())
             allCountriesSFR[,1:4]
           },
           protocol = {
             # summary protocol info
             Protocol_data          
           }
          
     )
  })
}) #shinyServer