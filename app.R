##
## TLS WEBAPP - PRACTITIONER VERSION
##
## UPDATED 03/16/2026
## Data input: one file per time period 
##             Similar to Bala's version of the app, except the first column of 'Zc' format 
##             includes target names
##              TargetID XYZ XYZ XYZ XYZ
##             [                        ]
##             [                        ]
##
##   (a separate app or function will create this single file format from the four individual position files, 
##    if necessary)
##


library(shiny)
library(shinythemes)
library(ICSNP)     ## for Hotellings T2 test
library(heplots)   ## for visualizing data ellipses
library(dplyr)     ## %>% transformations
library(geometry)  ## for 'cart2sph' function
library(MASS)      ## for mvrnorm() 
library(ggplot2)
library(tidyr)
library(tools)     ## for determining file extension
library(patchwork) ## for making the data ellipses plots (my version)
library(Cairo) ## for saving pdf with ascii characters (e.g., 'theta')
library(stringr)   ## for naming things ('str_pad' in "create_pretty_R()")
library(RColorBrewer)
library(shinyjs)   ## for making a discrete 'advanced options' button

## define where to get functions from
source('R/utils.R')  

##**USE THIS WHEN ON MY COMPUTER
#source("C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/R/utils.R")

## Generate a random seed that will display to the user, if they select 'Advanced settings'
rand_seed <- sample(1:1000000, 1)

##############################
##
## DEFINE THE UI
##
##############################
ui <- fluidPage(theme = shinytheme("spacelab"),
    ## Call 'shinyjs'
    useShinyjs(), # Initialize shinyjs

    # Application title
    titlePanel("OSAC TLS Performance Assessment WebApp"),

    # Data input box, with dropdown for units
    sidebarLayout(
        sidebarPanel(
          ## REMOVING THE OPTION TO RUN EXAMPLE FILES
          #radioButtons('upload_or_default', "Upload Files or Run Example?", 
          #             choices = c('Upload Files', 'Run Example Files'), 
          #             selected = 'Upload Files'),
          
      #conditionalPanel(
      #  condition = "input.upload_or_default == 'Upload Files'", 
          h2("1. Upload TLS Data"),
            fileInput('file_TLStest', "Test data", accept = c(".csv", ".txt")), 
            div(style = "margin-top: -20px"),
            textInput(inputId = "dataname_test",
                    label = "Test Data Nickname:",
                    value = "Test data"), 
            selectInput('units','TLS units',choices=c('mm', 'meters', 'inches')),
          
          h2("2. Upload Reference Lengths"),
          fileInput('filename_tapedata', "Dataset of Reference Lengths", accept = c(".csv", ".txt")), 
          div(style = "margin-top: -25px"),
          selectInput('units_tape','Reference length units',choices=c('mm', 'cm', 'inches')),
          h2("3. Error Reporting and Specification"),
          selectInput('report_as', 'Report errors as:', choices = c('Length error (mm)', 'Percentage of reference length')),
          selectInput('includeThreshold', "Set error specificaion value?", choices = c('No', 'Yes')),
          ## conditional numeric input that appears if 'includeThreshold' == Yes
          conditionalPanel(
            condition = "input.includeThreshold == 'Yes' && input.report_as == 'Length error (mm)'",
            numericInput("threshold_mm","Error specification (mm)",value = 1, step = 0.1, min = 0.1)
          ),
          conditionalPanel(
            condition = "input.includeThreshold == 'Yes' && input.report_as == 'Percentage of reference length'",
            numericInput("threshold_pct","Error specification (%)",value = 0.5, step = 0.01, min = 0.01)
          ),
        h2("4. Historic Comparison"), 
        selectInput('partII_htest', "Compare the current data to a previous set of data?", choices = c('No', 'Yes')),
        ## Conditional: if the hypothesis test is to be performed, the user now needs to upload more data
        conditionalPanel(
          condition = "input.partII_htest == 'Yes'", 
          h3("4a. Upload Historic TLS Data"),
          fileInput('file_TLSbase', "Historic data", accept = c(".csv", ".txt")), 
          div(style = "margin-top: -20px"),
          textInput(inputId = "dataname_base",
                    label = "Historic Data Nickname:",
                    value = "Historic data"),
          selectInput('historic_units','Historic TLS data units',choices=c('mm', 'meters', 'inches'))
        ),
        
        fluidRow(
          column(width = 6, 
                 actionButton('runAnalysis','Run Analysis', class = "btn-primary")
                 ), 
          column(width = 6, 
                 div(style = "text-align: right;",
                     actionLink("toggle_adv", "Advanced settings", 
                                style = "color: #888888; font-size: 0.9em;")
                 )
          )
        ),
        ## My 'advanced options' settings
        #actionLink("toggle_adv", "Advanced settings..."),
        
        # 3. The Hidden Section (Appears directly below the link)
        hidden(
          div(id = "advanced_section",
              style = "margin-top: 15px; padding-left: 10px; border-left: 2px solid #ddd;",
              numericInput("alpha", "Significance Threshold", value = 0.01, step = 0.01, min = .001),
              numericInput("seed_value", "MCS seed value", value = rand_seed)
          )
        ),
        #br(),
        #  actionButton('runAnalysis','Run Analysis', class = "btn-primary"), 
        ),
        

        # Prepare space for all the output
        mainPanel(
          tabsetPanel(
            id = 'results_tabs',
            selected = "Part I - Accuracy", 
                      br(),
                      tabPanel("Information", 
                               uiOutput("info")),
                      ## prepare the output space for "Part I - Analysis" tab 
                      tabPanel("Part I - Accuracy", 
                               h2("Accuracy Summary"), 
                               ## Summary statement
                               uiOutput("length_summary"), 
                               ## Summary table -- if threshold given and no values exceed, then there will be no table 
                               uiOutput("lengtherrors_subtable"),
                               br(),
                               ## Part I - plot
                               plotOutput("errorPlot", width = "700px", height = "350px"),
                               br(),
                               ## All the table of errors and statements for Lengths A-F
                               #h5("Length details"), 
                                uiOutput("statement_A"),
                                #verbatimTextOutput("errorTable_A"),
                                uiOutput("statement_B"),
                                uiOutput("statement_C"),
                                uiOutput("statement_D"),
                                uiOutput("statement_E"),
                                uiOutput("statement_F"), 
                                br()
                                #verbatimTextOutput("errorTable_F")
                               ),
                      ## prepare the output space for "Part II - Analysis" tab 
                      tabPanel("Part II - Precision",
                            ## if a historic comparison is being made, the first thing to show up should be the 
                            ## results of the statistical hypothesis test
                              h2("Precision Summary"),
                            ## ** put the MCS results here
                              uiOutput("expectederrors_summary"), 
                              #verbatimTextOutput("expectederrors_subtable"),
                              uiOutput("expectederrors_subtable"),
                              br(),
                              plotOutput("expectederrorPlot", width = "700px", height = "350px"),
                              br(),
                            ## regardless of historical data, print the standard deviations from the Data Under Test
                               #h3("TLS Angular Precision - data under test"),
                               uiOutput("testdata_SDstatements")

                              ##**COVARIANCE MATRICES CAN BE PUT IN THE REPORT*
                               #h3("Covariance matrices"), 
                               #h5("Covariance matrix - data under test"),
                               #verbatimTextOutput("SigmaHat2"),
                  
                            #conditionalPanel(condition = "input.partII_htest == 'Yes'",
                            #               h5("Covariance matix - historic data"),
                            #               verbatimTextOutput("SigmaHat1")
                            #   ),
                            #uiOutput("covMat_info")
                      ),
                      ## 
                      ## Historic comparison Will appear here by Server code
                      ##

                      ## 
                      ## DATA SUMMARY TAB - report # of Targets/Postions, and plots that check for bad targets
                      ##
                      tabPanel("Data Summary & Report", 
                               h2("Download Report"),
                               downloadButton("download_report", "Generate Report"),
                               h3("Data Summary"),
                               ## data summary statement for TLS data under test
                               textOutput("testdata_summary"), 
                               ## data summary statement for reference lengths (tape measure)
                               textOutput("tapedata_summary"), 
                               ## Now some plots to check for bad targets
                               uiOutput("residualStatement"),
                               plotOutput("resPlot_test", width = "600px", height = "600px"),
                               ## Now report summary stuff for the Historic Data (if uploaded)
                               conditionalPanel(condition = "input.partII_htest == 'Yes'",
                                                h3("Historic Data Summary"),
                                                textOutput("basedata_summary"),
                                                plotOutput("resPlot_base", width = "600px", height = "600px"))
                               )
          )
        )
    )
)


##############################
##
## DEFINE THE SERVER LOGIC
##
##############################
server <- function(input, output, session) {
  
  ## DETERMINE IF WE NEED THE HISTORIC COMPARISON TAB
  observeEvent(input$partII_htest, {
    if (input$partII_htest == "Yes") {
      insertTab(
        inputId = "results_tabs",
        tabPanel(
          title = "Comparison to Historic Data",
          value = "historic_tab", # This ID is used to remove it later
          h2("Accuracy Comparison"),
          # Add whatever outputs you need here, e.g.:
          # plotOutput("historicDetailedPlot")
          h2("Precision Comparison"), 
          uiOutput("p2_interpretation"), 
          br(),
          plotOutput("ellipsePlot", width = "600px", height = "600px"),
          br(),
          uiOutput("basedata_SDstatements")
        ),
        target = "Part II - Precision", # Place it after this tab
        position = "after"
      )
    } else {
      # This removes the tab if the user selects "No"
      removeTab(inputId = "results_tabs", target = "historic_tab")
    }
  })
  
  ## OBSERVE IF WE NEED THE ADVANCED SETTINGS OPTION
  observeEvent(input$toggle_adv, {
    toggle("advanced_section", anim = TRUE)
  })
  
  ## IMPORT THE TLS TEST DATA 
  data_test = reactive({
    if(is.null(input$file_TLStest)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$file_TLStest$name)
      data_test = switch(ext, csv = read.csv(input$file_TLStest$datapath), 
                           txt = read.table(input$file_TLStest$datapath, header = FALSE), 
                           validate("Invalid file; Please upload a .csv or .txt file"))
      return(data_test)
    }
  })
  
  ## IMPORT THE BASELINE DATA 
  data_base = reactive({
    if(is.null(input$file_TLSbase)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$file_TLSbase$name)
      data_base = switch(ext, csv = read.csv(input$file_TLSbase$datapath), 
                         txt = read.table(input$file_TLSbase$datapath, header = FALSE), 
                         validate("Invalid file; Please upload a .csv or .txt file"))
      return(data_base)
    }
  })
  

  ##
  ## IMPORT THE TAPE MEASURE DATA
  ##
  Dtape = reactive({
    if(is.null(input$filename_tapedata)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$filename_tapedata$name)
      Dtape = switch(ext, csv = read.csv(input$filename_tapedata$datapath), 
                           txt = read.table(input$filename_tapedata$datapath, header = TRUE), 
                           validate("Invalid file; Please upload a .csv or .txt file"))
      
      #filename_tapedata = input$filename_tapedata
      #Dtape = read.table(filename_tapedata$datapath, header = TRUE)
      return(Dtape)
    }
  })
  

  ## 
  ## NOW DO STUFF, WHEN THE BUTTON IS CLICKED
  ##
  all_results <- eventReactive(input$runAnalysis, {
    
    ##**CHECK IF DATA FILES WERE UPLOADED**
    ##*
    
    ## ESTABLISH PROGRESS BAR
   withProgress(message = 'Performing', value = 0, {
    
    results_out = list()
    
    
    ## USE THIS TO DEBUG
    #browser()
    
    ## GET NAMES FOR THE DATASETS
    dataname_base <- input$dataname_base
    dataname_test <- input$dataname_test
    ## and also save these to the 'results_out' so I can call them in plots later 
    ## (yes, having both the above and below is inefficient, but I don't want to break anything)
    results_out$dataname_test <- input$dataname_test
    results_out$dataname_base <- input$dataname_base
    
    ## READ IN THE TLS TEST DATA
    #results_out$data2 = data_test()

    ## CONVERT TLS TEST DATA TO mm, IF NECESSARY
    Zc2_named = convertZc_tomm(data_test(), units = input$units)
    data2 = Zc2_named[,-1]  ## remove the 'target_list' column
    data2_targetList = Zc2_named[,1] ## seperate out the list of target names
    ## save the data in 'results_out'
    results_out$data2 = data2
    
    ##**************************
    ## DATA SUMMARY STATEMENTS
    ##
    # GET UNITS FOR TLS DATA UNDER TEST AND TAPE
    results_out$units = input$units
    results_out$units_tape = input$units_tape
    
    ## 1) Create data summary statement for TLS data under test
    TLS_UnitStatement = if(input$units=='mm') {
      paste("The", paste0('"',dataname_test,'"'), "coordinates were collected in mm.")
    } else {paste("The", paste0('"',dataname_test,'"'), "coordinates were collected in", input$units, "and converted to mm.")
    }
    nTP_test <- TLS_getTandP(data2)
    results_out$testdata_summary <- paste("The", paste0('"',dataname_test,'"'), "data are Cartesian coordinates from", nTP_test$nTargets, "targets 
                                            measured from", nTP_test$nPositions, "TLS positions.", 
                                          TLS_UnitStatement)
    ## 2) Create data summary statement for Tape Measure data
    results_out$tapedata_summary = if(input$units_tape=='mm') {
      paste("Reference lengths were collected in mm. Length errors are in mm.")
    } else {paste("Reference lengths were collected in", input$units_tape, "and converted to mm. Length errors are in mm.")
    }
    

    ##
    ##**Conditional that historical data is being used:
    ##*  a. create the 'data1' file
    ##*  b. calculate the number of targets/positions
    ##*  c. create the historical data summary statement
    ##*  d. save the data 1 in the 'results_out' stuff
    if(input$partII_htest=="Yes") {
      ## a) create the 'data1' file, converting to mm if necessary
      Zc1_named = convertZc_tomm(data_base(), units = input$units)
      data1 = Zc1_named[,-1]
      ## b) determine what the units are
      historicTLS_UnitStatement = if(input$historic_units=='mm') {
        paste("The", paste0('"',dataname_base,'"'),  "coordinates were collected in mm.")
      } else {paste("The", paste0('"',dataname_base,'"'), "coordinates were collected in", input$historic_units, "and converted to mm.")
      }
      ## c) calculate the number of targets/positions
      nTP_base = TLS_getTandP(data1)
      ## d) create/save the data summary statement for the historical TLS data
      results_out$basedata_summary <- paste("The", paste0('"',dataname_base,'"'), "data are Cartesian coordinates from", nTP_base$nTargets, "targets 
                                            measured from", nTP_base$nPositions, "TLS positions.", 
                                            historicTLS_UnitStatement)
      ## e) save the data 1 in the 'results_out' collection
      results_out$data1 = data1
      }
    
    ## READ IN THE TAPE MEASURE DATA
    Dtape = Dtape()
    ## If tape_units are not already mm, convert to mm (unit options are mm, cm or inches)
    Dtape = Dtape_ToMM(dat = Dtape, units = input$units_tape)
    
    
    #browser()
    ## DETERMINE IF AN ERROR THRESHOLD WAS SET...this could either be given in mm or as a %
    ## There is no longer a need to convert the threshold to mm...it must be given as mm or %
    ## first, convert threshold to mm, if necessary
    ## if includeThreshold==No, make NA,
    ## otherwise, need to determine the units_tape and see if the threshold needs to change
    results_out$threshold = if(input$includeThreshold=='No') {
      NA} else{
        switch(input$report_as, 
               'Length error (mm)' = input$threshold_mm, 
               'Percentage of reference length' = input$threshold_pct
        )
      }
    
    ## this can be fed directly into the Part I plotting function
    results_out$error_reporting = switch(input$report_as, 
              'Length error (mm)' = 'mm', 
              'Percentage of reference length' = 'pct')

    ##*********************
    ## PART I CALCULATIONS
    ##*********************
    incProgress(1/7, detail = "Part I analysis")
    ## CHECK IF TAPE DATA UNITS ARE MM, AND IF NOT CONVERT THEM TO MM
    ## ... 
    ## Calculate the 4 length errors (one per position) for the six lengths

    A_F_LengthError_list = lengthErrors(Dtape = Dtape, Zc = data2, Zc_names = data2_targetList)
    ## Get the table of errors and table of statements
    results_out$A_F_LengthError_table = A_F_LengthError_list$table
    A_F_LengthError_statements = A_F_LengthError_list$statements
    results_out$A_F_LengthError_statements = A_F_LengthError_list$statements
    
    ## LENGTH A
    results_out$errorTable_A = subset(results_out$A_F_LengthError_table, ID=="A")
    results_out$errorStatement_A = subset(A_F_LengthError_statements, ID=="A")$statement
    ## LENGTH B
    results_out$errorTable_B = subset(results_out$A_F_LengthError_table, ID=="B")
    results_out$errorStatement_B = subset(A_F_LengthError_statements, ID=="B")$statement
    ## LENGTH C
    results_out$errorTable_C = subset(results_out$A_F_LengthError_table, ID=="C")
    results_out$errorStatement_C = subset(A_F_LengthError_statements, ID=="C")$statement
    ## LENGTH D
    results_out$errorTable_D = subset(results_out$A_F_LengthError_table, ID=="D")
    results_out$errorStatement_D = subset(A_F_LengthError_statements, ID=="D")$statement
    ## LENGTH E
    results_out$errorTable_E = subset(results_out$A_F_LengthError_table, ID=="E")
    results_out$errorStatement_E = subset(A_F_LengthError_statements, ID=="E")$statement
    ## LENGTH F
    results_out$errorTable_F = subset(results_out$A_F_LengthError_table, ID=="F")
    results_out$errorStatement_F = subset(A_F_LengthError_statements, ID=="F")$statement
    
    ## If a threshold was included, count how many absolute length errors are larger
    ## than the threshold, and report the values that were, if applicable
    ## 

    if(input$includeThreshold=='No') {
      ## IF NO THRESHOLD IS PROVIDED, PROVIDE A TABLE OF THE LARGEST THREE ERRORS
      ## the Statement and the table will change, depending on whether the user wants the results
      ## reported as length errors or as % error...
      results_out$lengtherrors_summary = switch(input$report_as, 
             'Length error (mm)' = "No error specification was provided. Displaying the three largest length errors.",
             'Percentage of reference length' = "No error specification was provided. Displaying the three largest percent errors."
      )
      results_out$lengtherrors_subtable = switch(input$report_as,
              'Length error (mm)' = (results_out$A_F_LengthError_table %>% arrange(desc(abs(Error))))[1:3,], 
              'Percentage of reference length' = (results_out$A_F_LengthError_table %>% arrange(desc(abs(PctError))))[1:3,]                                    
      )
    } else{
      ## First make a generic version of the lengthError table so I can feed it into my 'statements' function
      gtab_PartI <- A_F_LengthError_list$table %>% 
        reframe(ID, ReferenceLength, Position, Length, 
                y = Error, y_pct = PctError)
      ## use the 'statements' function on the generic table
      lengtherrors_exceed = lengthError_statement(table = gtab_PartI, t_val = results_out$threshold, 
                                                  report_as = results_out$error_reporting, section = "PartI")
      results_out$lengtherrors_summary = lengtherrors_exceed$statement
      results_out$lengtherrors_subtable = lengtherrors_exceed$table
    }
    
    
    
    ##*********************
    ## PART II CALCULATIONS
    ##*********************
    ## Step 1:
    ## transform the Cartesian coordinates into spherical residuals
    incProgress(2/7, detail = "rigid body transformation on test data")
    results_out$R2 = Zc_to_R(data2)
    
    ## Step 2:
    ## Transform the residuals from wide to long
    results_out$X2 = RtoX(results_out$R2)
    
    ## CALCULATE THE COVARIANCE MATRIX FROM THE DATA UNDER TEST
    results_out$SigmaHat2 = cov(results_out$X2)

    ## create the statements about the standard deviations in the angular/ranging residuals.
    testdata_sd = round(sqrt(diag(round(results_out$SigmaHat2,3))),2)
    results_out$testdata_SDstatements = HTML(
      paste("The standard deviation in the azimuth angle residuals is", testdata_sd[1], "arcsec.<br>", 
            "The standard deviation in the elevation angle residuals is", testdata_sd[2], "arcsec.<br>",
            "The standard deviation in the ranging residuals is", testdata_sd[3], "mm."))
    
    ##*********************
    ##* MCS ---  PROPAGATE SIGMAHAT TO LENGTHS
    ##*********************
    incProgress(3/7, detail = "Monte Carlo simulation - this will take some time")
    # 1. Initialize the SECOND (inner) progress bar
    # We set max to nIt so the value represents the actual iteration count
    mc_progress <- shiny::Progress$new(session, min = 1, max = 3000)
    mc_progress$set(message = "Running Monte Carlo", value = 0)
    
    # Close the inner bar automatically when this block finishes or fails
    on.exit(mc_progress$close())
    
    # 2. Call your function, passing the progress object
    expected_errors <- obtain_expected_errors(Zc = Zc2_named, 
                                                          ref_lengths = Dtape, SigmaHat = results_out$SigmaHat2, 
                                                          nIt = 1500, EE_scale = 3,
                                              progress_obj = mc_progress)  # Pass the object here
    incProgress(4/7, detail = "MCS finished")
    ## now add in the column names that the plotting function will expect
    results_out$expected_errors = expected_errors %>%
      mutate(expectederror_pct = round(expected_error/ReferenceLength*100,2))
    
    ## If a threshold was included, count how many expected errors are larger
    ## than the threshold, and report the values that were, if applicable
    ## 
    if(input$includeThreshold=='No') {
      ## IF NO THRESHOLD IS PROVIDED, PROVIDE A TABLE OF THE LARGEST THREE ERRORS
      ## the Statement and the table will change, depending on whether the user wants the results
      ## reported as length errors or as % error...
      results_out$expectederrors_summary = switch(input$report_as, 
                                                'Length error (mm)' = "No error specification was provided. Displaying the three largest maximum expected length errors.",
                                                'Percentage of reference length' = "No error specification was provided. Displaying the three largest maximum expected errors as a percent of reference length."
      )
      results_out$expectederrors_subtable = switch(input$report_as,
                                                 'Length error (mm)' = (results_out$expected_errors %>% arrange(desc(abs(expected_error))))[1:3,], 
                                                 'Percentage of reference length' = (results_out$expected_errors %>% arrange(desc(abs(expectederror_pct))))[1:3,] 
      )
    } else{
      ## First make a generic version of the expectederror table so I can feed it into my 'statements' function
      gtab_MCS <- results_out$expected_errors %>% 
        reframe(ID, ReferenceLength, Position,
                y = expected_error, y_pct = expectederror_pct)
      ## use the 'statements' function on the generic table
      expectederrors_exceed = lengthError_statement(table = gtab_MCS, t_val = results_out$threshold, 
                                                  report_as = results_out$error_reporting, section = "MCS")
      results_out$expectederrors_summary = expectederrors_exceed$statement
      results_out$expectederrors_subtable = expectederrors_exceed$table
    }
    
    ## CHECK FOR BAD TARGETS -- punt the 'R_pretty' dataset for the Historic dataset, if applicable
    results_out$Rpretty_test <- create_pretty_R(results_out$R2, data_name = dataname_test)

    
    
    ##*********************
    ##**PART II -- CONDITIONAL ON WHETHER HISTORICAL COMPARISON IS BEING DONE**
    ##* Do the same Part II on the historical ('baseline') data
    if(input$partII_htest=="Yes") {
      incProgress(5/7, detail = "rigid body transformation on historic data")
      ## Transform Cartesian coordinates to spherical residuals
      results_out$R1 = Zc_to_R(data1)
      ## transform from wide to long
      results_out$X1 = RtoX(results_out$R1)
      ## estimate the covariance matrix
      results_out$SigmaHat1 = cov(results_out$X1)
      
      ## get statement on the historic standard deviations
      basedata_sd = round(sqrt(diag(round(results_out$SigmaHat1,3))),2)
      results_out$basedata_SDstatements = HTML(
        paste("The historical standard deviation in the azimuth angle residuals is", basedata_sd[1], "arcsec.<br>", 
              "The historical standard deviation in the elevation angle residuals is", basedata_sd[2], "arcsec.<br>", 
              "The historical standard deviation in the ranging residuals is", basedata_sd[3], "mm."))
      
      ## Step 3:
      ## Perform the hypothesis test
      ## Step 3: test equality of the covariance matrices
      incProgress(6/7, detail = "Part II analysis")
      alpha <- 0.01
      p2_results = TLS_cov_check(results_out$X1, results_out$X2, conf.level = 1-alpha)
      results_out$test_results = p2_results$results
      results_out$p2_conclusion = p2_results$conclusion
      #results_out$p2_interpretation = p2_results$interpretation
      pval_clean = ifelse(p2_results$pvalue < 0.001, "<0.001", round(p2_results$pvalue,3))
      
      ## full statement on hypothesis test results
      results_out$p2_interpretation = paste("The statistical methodology detailed in <a href='https://doi.org/10.1111/1556-4029.70256' target='_blank'>Gregg et al. (2026)</a> 
                                         applied to the uploaded data results in a p-value of", pval_clean, ".", p2_results$interpretation)  
                                        
    
      ## Step 4:
      ## Combine the data in preparation for plotting the data ellipses
      Xcomb <- rbind(data.frame(results_out$X1, dataset = dataname_base), 
                   data.frame(results_out$X2, dataset = dataname_test))
      Xcomb$dataset <- factor(Xcomb$dataset, levels = unique(Xcomb$dataset))

    ## Need to remove any NA values
       if(sum(is.na(Xcomb[,1]))>0) {
        Xcomb <- Xcomb[-c(which(is.na(Xcomb[,1]))),]
       }
      ##**REMOVE THE RANGING RESIDUAL COLUMN, IF WE'RE JUST TESTING ANGULAR RESIDUALS
      #results_out$Xcomb = Xcomb[,-3]
      results_out$Xcomb = Xcomb
      
      ## CHECK FOR BAD TARGETS -- punt the 'R_pretty' dataset for the historical data, if applicable 
      results_out$Rpretty_base <- create_pretty_R(results_out$R1, data_name = dataname_base)
      
    }
    
    return(results_out)
  })
#browser()
  })

  
  ###########################
  ##**PART 1 ANALYSIS
  ##*
  #output$units_tape <- renderText({
  #  if(is.null(all_results()$tape_UnitStatement)) {
  #    return(NULL)
  #  }
  #  return(paste(all_results()$TLS_UnitStatement, all_results()$tape_UnitStatement))
  #})
  
  ## Give statement on Historical TLS data units, if it was used
  output$units_historical <- renderText({
    if(is.null(all_results()$units_historical)) {
      return(NULL)
    }
    return(paste(all_results()$units_historical))
  })
  #output$units_tape <- renderText({
  #  if(is.null(all_results()$units_tape)) {
  #    return(NULL)
  #  }
  #  units_tape = all_results()$units_tape
  #  if(units_tape=='mm') {
  #    out_str = paste("Tape measure units are mm.")
  #  } else {
  #    out_str = paste("Tape measure units have been converted from ", units_tape, "to mm.")
  #  }
  #  return(out_str)
  #})
  
  ## Overall statement "x out of y length errors are greater than *threshold*"
  output$length_summary <- renderUI({
    if(is.null(all_results()$lengtherrors_summary)) {
      return(NULL)}
    return(all_results()$lengtherrors_summary)
  })
  ## and the corresponding table
  #output$lengtherrors_subtable <- renderUI({
  #  if (nrow(all_results()$lengtherrors_subtable) > 0) {
  #    # Return the verbatim output only if data exists
  #    verbatimTextOutput(all_results()$lengtherrors_subtable)
  #  } else {
      # Return nothing, or a simple text message
      # return(p("No threshold errors detected.")) 
  #    return(NULL)
  #  }
  #})
  
  ## Creating a 'conditional' table for the Part I results (no table reported if threshold given and nothing exceeds threshold)
  # 1. The Dynamic UI Container
  output$lengtherrors_subtable <- renderUI({
    # Check if the table has data
    if (nrow(all_results()$lengtherrors_subtable) > 0) {
      # Create the placeholder with a unique ID string
      verbatimTextOutput("actual_table_content")
    } else {
      return(NULL)
    }
  })
  # 2. The Actual Content Provider
  output$actual_table_content <- renderPrint({
    # This sends the actual data to the placeholder created above
    all_results()$lengtherrors_subtable
  })
  
  ## The individual length tables
  output$statement_A <- renderUI({
    if(is.null(all_results()$errorStatement_A)) {
      return(NULL)}
    return(all_results()$errorStatement_A)
  })
  output$errorTable_A <- renderPrint({
    if(is.null(all_results()$errorTable_A)) {
      return(NULL)}
    return(print(all_results()$errorTable_A, row.names = F))
  })
  
  output$statement_B <- renderUI({
    if(is.null(all_results()$errorStatement_B)) {
      return(NULL)}
    return(all_results()$errorStatement_B)
  })
  output$errorTable_B <- renderPrint({
    if(is.null(all_results()$errorTable_B)) {
      return(NULL)}
    return(print(all_results()$errorTable_B, row.names = F))
  })
  
  output$statement_C <- renderUI({
    if(is.null(all_results()$errorStatement_C)) {
      return(NULL)}
    return(all_results()$errorStatement_C)
  })
  output$errorTable_C <- renderPrint({
    if(is.null(all_results()$errorTable_C)) {
      return(NULL)}
    return(print(all_results()$errorTable_C, row.names = F))
  })
  
  output$statement_D <- renderUI({
    if(is.null(all_results()$errorStatement_D)) {
      return(NULL)}
    return(all_results()$errorStatement_D)
  })
  output$errorTable_D <- renderPrint({
    if(is.null(all_results()$errorTable_D)) {
      return(NULL)}
    return(print(all_results()$errorTable_D, row.names = F))
  })
  
  output$statement_E <- renderUI({
    if(is.null(all_results()$errorStatement_E)) {
      return(NULL)}
    return(all_results()$errorStatement_E)
  })
  output$errorTable_E <- renderPrint({
    if(is.null(all_results()$errorTable_E)) {
      return(NULL)}
    return(print(all_results()$errorTable_E, row.names = F))
  })
  
  output$statement_F <- renderUI({
    if(is.null(all_results()$errorStatement_F)) {
      return(NULL)}
    return(all_results()$errorStatement_F)
  })
  output$errorTable_F <- renderPrint({
    if(is.null(all_results()$errorTable_F)) {
      return(NULL)}
    return(print(all_results()$errorTable_F, row.names = F))
  })
  
  ##############################
  ## PART I - PLOT THE ERRORS
  ## updated to account for needing to do this in both webapp and report

  # 1. Create the plot as a reactive object
  error_plot_reactive <- reactive({
    # Check if data exists
    if(is.null(all_results()$A_F_LengthError_table)) {
      return(NULL)
    }
    
    # Call your custom plotting function
    partIerrorplot(
      dat = all_results()$A_F_LengthError_table, 
      threshold = all_results()$threshold, 
      plot_as = all_results()$error_reporting, 
      my_title = all_results()$dataname_test
    )
  })
  
  # 2. Render the plot in the web app (later we will render it in the downloadable report format)
  output$errorPlot <- renderPlot({
    error_plot_reactive()
  }, res = 96)
  

  
  #############################
  ##** PART II ANALYSIS
  ##*
  ## FIRST PRINT THE COVARIANCE MATRIX FROM THE TLS ('test') DATA
  
  ## The estimated variance/covariance matrices
    output$covMat_info <- renderUI({
      if(is.null(all_results()$SigmaHat2)) {
        return(NULL)
      }
  ## state some info on the estimated covariance matrices 
      HTML(paste("theta = azimuth angle (arcsec)<br>", 
                 "phi = elevation angle (arcsec)<br>",
                 "r = ranging direction (mm)"
                 ))
    })

    output$SigmaHat2 <- renderPrint({
      if(is.null(all_results()$SigmaHat2)) {
        return(NULL)
      }
      return(round(all_results()$SigmaHat2,3))
    })
    ## statement about the standard deviations
    output$testdata_SDstatements <- renderUI({
      if(is.null(all_results()$testdata_SDstatements)) {
        return(NULL)
        }
      return(all_results()$testdata_SDstatements)
    })
    
     output$SigmaHat1 <- renderPrint({
        if(is.null(all_results()$SigmaHat1)) {
          return(NULL)
        }
        return(round(all_results()$SigmaHat1,3))
      })
     ## statement about the standard deviations
     output$basedata_SDstatements <- renderUI({
       if(is.null(all_results()$basedata_SDstatements)) {
         return(NULL)
       }
       return(all_results()$basedata_SDstatements)
     })
  

  ## The string of text stating the hypothesis test conclusion
  output$p2_conclusion <- renderText({
    if(is.null(all_results()$p2_conclusion)) {
      return(NULL)
    }
    return(all_results()$p2_conclusion)
  })
  
  output$p2_interpretation <- renderUI({
    if(is.null(all_results()$p2_interpretation)) {
      return(NULL)
    }
    return(HTML(all_results()$p2_interpretation))
  })
  
  ## The information from the Hotellings T2 test
  output$p2_analysis <- renderPrint({
    if(is.null(all_results()$test_results)) {
      return(NULL)
    }
    return(all_results()$test_results)
  })
  

  ###############################
  ##**PART II - DATA ELLIPSES
  output$ellipsePlot <- renderPlot({
    if(is.null(all_results()$Xcomb)) {
      return(invisible(NULL))
    }
    my_ellipse_plot(all_results()$Xcomb)
  }, res = 96)
  
  #output$ellipsePlot <- renderPlot({
  #  if(is.null(all_results()$Xcomb)) {
  #    return(invisible(NULL))
  #  }
  #  ggplot(all_results()$plot_data, aes(x=x1, y=x2))+ 
  #    geom_point()
  #  covEllipses(all_results()$Xcomb[,1:3], as.factor(all_results()$Xcomb[,4]), variables=1:3, pooled = FALSE, 
  #              label.pos = c("center", "top"), col = c("darkred", "dodgerblue"))
  #}, res = 96)
  
  ###############################
  ## MCS RESULTS
  ## State how many, if any, expected errors exceed the threshold
  ## then
  ## Plot the 'expected errors' in the 24 lengths, derived from the MCS

  ## Overall statement "x out of y length errors are greater than *threshold*"
  output$expectederrors_summary <- renderUI({
    if(is.null(all_results()$expectederrors_summary)) {
      return(NULL)}
    return(all_results()$expectederrors_summary)
  })

  ## Creating a 'conditional' table for the Expected Errors
  # 1. The Dynamic UI Container
  output$expectederrors_subtable <- renderUI({
    # Check if the table has data
    if (nrow(all_results()$expectederrors_subtable) > 0) {
      # Create the placeholder with a unique ID string
      verbatimTextOutput("actual_table_content_EE")
    } else {
      return(NULL)
    }
  })
  # 2. The Actual Content Provider
  output$actual_table_content_EE <- renderPrint({
    # This sends the actual data to the placeholder created above
    all_results()$expectederrors_subtable
  })

  # 1. Create the plot as a reactive object
  expectederror_plot_reactive <- reactive({
    # Check if data exists
    if(is.null(all_results()$expected_errors)) {
      return(NULL)
    }
    
    # Call your custom plotting function
    plot_expectederrors(
      dat = all_results()$expected_errors, 
      threshold = all_results()$threshold, 
      plot_as = all_results()$error_reporting,
      my_title = all_results()$dataname_test
    )
  })
  
  # 2. Render the plot in the web app (later we will render it in the downloadable report format)
  output$expectederrorPlot <- renderPlot({
    expectederror_plot_reactive()
  }, res = 96)
  
  ###############################
  ## Data Summaries
    output$testdata_summary <- renderText({
      if(is.null(all_results()$testdata_summary)) {
        return(NULL)
      }
      return(paste(all_results()$testdata_summary))
    })
     
     output$tapedata_summary <- renderText({
       if(is.null(all_results()$tapedata_summary)) {
         return(NULL)
       }
       return(paste(all_results()$tapedata_summary))
     })
    
    ## Historical TLS data summary
    output$basedata_summary <- renderText({
      if(is.null(all_results()$basedata_summary)) {
        return(NULL)
      }
      return(paste(all_results()$basedata_summary))
    })
    
    ################################
    ## CHECK FOR BAD TARGETS
    ## 1) generate the lsit of plots
    ## 2) plot the plots
    output$resPlot_base <- renderPlot({
      if(is.null(all_results()$Rpretty_base)) {
        return(NULL)
      }
      plot_my_residuals(all_results()$Rpretty_base)
    }, res = 96)
    
    output$resPlot_test <- renderPlot({
      if(is.null(all_results()$Rpretty_test)) {
        return(NULL)
      }
      plot_my_residuals(all_results()$Rpretty_test)
    })
    
    output$residualStatement <- renderUI({
      HTML(paste("Angular residuals are reported in arcseconds. Ranging residuals are reported in mm."
      ))
    })
  
    
  ###############################
  ##**Create Report
  ##*
    output$download_report <- downloadHandler(
      # 1. Define the filename
      filename = function() {
        paste0("report-", Sys.Date(), ".pdf")
      },
      # 2. Define the content generation
      content = function(file) {
        # Copy the report file to a temporary directory
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        # You can pull these from input$ or reactive variables
        params <- list(
          report_title = "TLS WebApp Results",
          ## PART I RESULTS
          length_summary = all_results()$lengtherrors_summary,
          error_plot = error_plot_reactive(), # The new plot reactive
          error_table = all_results()$A_F_LengthError_table, 
          lengtherror_statements = all_results()$A_F_LengthError_statements,
          ## PART II RESULTS
          ## --tbd--
          
          ## MCS RESULTS
          expectederror_plot = expectederror_plot_reactive()
        )
        
        # Knit the document
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
      
  ###############################
  ##**Information
  ##*
  output$info <- renderUI({
    HTML(paste("This web app implements the required calculations 
    to perform the OSAC CSIR `Terrestrial Laser Scanner Performance Assessment Test Procedure'. 
    This test procedure consists of two parts:<br><br>", 
    "Part I: Test of the instrument's target-to-target accuracy;<br>", 
    "Part II: Statement of the instrument's spherical precision.<br><br>", 
    "An optional addition to Part II allows the user to test if the instrument's spherical precision has significantly changed from a historic data set.<br><br>",
    
    "The user manual for this webapp can be downloaded [here]. Additional information can be found in the OSAC standard, [this paper], and [this other paper]."))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
