library(ggvis)

shinyUI(
  
  fluidPage(
 
  titlePanel(StudyTitle),
  sidebarLayout(
    sidebarPanel( width=3
     
      ,conditionalPanel(
        condition = "input.tabs1=='Lab Tests'",
           sliderInput("tVal", "Test value",0,50000, c(0, 2000), step = 10)
           ,radioButtons("idisplayresults", "Display Data Value as:",
                   c("Actual" = "actualDataValue",
                     "Normalized by protocol mean" = "protocolMeanDataValue",
                     "Normalized by country mean" = "countryMeanDataValue",
                     "Normalized by site mean" = "siteMeanDataValue"))
      )
      ,conditionalPanel("input.tabs1 != 'Summary' && input.tabs1 != 'SFR'"
           ,radioButtons("ipatienttype", "Patients:",
                      c("All" = "all",
                        "Screen Passed Only" = "screenpassed",
                        "Screen Failed Only" = "screenfailed"))
      )
      ,conditionalPanel(
        condition = ("input.tabs1=='Box Plots' || input.tabs1=='SFR'"),
                     radioButtons("iby", "Select by: ",
                                  c("By Country" = "country",
                                    "By Site" = "site"))
      )
      ,conditionalPanel(
        condition = "input.tabs1=='Summary'",
        radioButtons("isummary", "Select summary: ",
                     c("Test" = "test",
                       "Site" = "site",
                       "Country" = "country",
                       "Patient" = "patient"
                       ))
      )
      ,conditionalPanel(
        condition = "input.tabs1=='Histogram'",
        radioButtons("ihistogram", "Select type: ",
                     c("Test Values" = "test",
                       "Protocol Deviations" = "protocoldeviation",
                       "Patient Age" = "age",
                       "Patient Gender" = "gender"
                     ))
      )
      ,conditionalPanel(
        condition = "input.tabs1=='Densities'",
        radioButtons("idensity", "Select type: ",
                     c("Test Values" = "test",
                       "Patient Age" = "age",
                       "Patient Gender" = "gender"
                     )),
        sliderInput("ibandwidth","Bandwidth adjustment", .1, 2, value = 1, step = .1 ),
        selectInput("ikernel","kernel",choices=
                               c("Gaussian" = "gaussian",
                                 "Epanechnikov" = "epanechnikov",
                                 "Rectangular" = "rectangular",
                                 "Triangular" = "triangular",
                                 "Biweight" = "biweight",
                                 "Cosine" = "cosine",
                                 "Optcosine" = "optcosine")
                              )
       )
      ,conditionalPanel(
        condition = "input.tabs1=='CDF'",
        radioButtons("icdf", "Select type: ",
                     c("Test Values" = "test",
                       "Patient Age" = "age"
        ))
        
      )
      ,conditionalPanel(
        condition = "input.tabs1!='Summary'"
        ,selectInput("itname",
                     "Select Tests:",
                     choices = TestNames,multiple=TRUE)
        
      )
      ,conditionalPanel(
        condition = "input.tabs1=='Outliers'"
          ,textInput("inumoutliers","Number of Outliers greater than:",value=1)
          ,checkboxInput("ipatientnorm", "Divide by number of patients", TRUE)
          ,radioButtons("ioutliertype", "Select outlier type: ",
                     c("Site outliers per test" = "sitepertest",
                       "Site outliers total" = "sitetotal",
                       "Test outliers" = "test",
                       "Country outliers" = "country",
                       "Patient outliers" = "patient"
                     ))
        
      )
      ,conditionalPanel(
        condition = "input.tabs1!='Summary'"
                     
                        ,selectInput("ipiname",
                                    "Site Name:",
                                    choices = InvestigatorNames,multiple=TRUE)
                        ,selectInput("icountry",
                                    "Country:",
                                    choices = CountryNames,multiple=TRUE)
                     
                        ,selectInput("igender", "Gender",c("Both","F","M"))
      
                        ,sliderInput("iage", "Patient Age",0,150, c(0, 150), step = 1) 
        )
        
    ),
    
    mainPanel( 
      tabsetPanel(id="tabs1",
                  
        tabPanel("Lab Tests",ggvisOutput("plotLabResults"), 
                 wellPanel(span("(this may take a while) Number of results:",textOutput("n_tests") ))),
        tabPanel("SFR", ggvisOutput("plotSFR")),
        tabPanel("Box Plots", ggvisOutput("plotBoxPlot")),
        tabPanel("Summary", dataTableOutput('summary')),
        tabPanel("Histogram", ggvisOutput("plotHistogram")),
        tabPanel("Densities", ggvisOutput("plotDensities")),
        tabPanel("Outliers", ggvisOutput("plotOutliers")),
        tabPanel("CDF", ggvisOutput("plotCDF")),
        tabPanel("R Session", fluidRow(column(12,wellPanel("The last query results are in Last_query_results.  Use objects() or ls() to see other available objects."))                 )
                             ,fluidRow( column(2, br(), actionButton("submitButton", "Submit",style="background-color:#aac2d6"))
                                       ,column(6, textInput("iRstatement","",value=""))
                              )
                             ,wellPanel(style="height:600px;overflow-y:scroll; max-height: 600px",htmlOutput("nText")))
        
        
      )
   )
  )
))