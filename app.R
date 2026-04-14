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

## define where to get functions from
source('R/utils.R')  

##**USE THIS WHEN ON MY COMPUTER
#source("C:/Users/meg3/OneDrive - NIST/Work/OSAC/CSIR subcommitee/TLS/code/WebApp/TLSwebapp_PractitionerVersion/R/utils.R")


##############################
##
## DEFINE THE UI
##
##############################
ui <- fluidPage(theme = shinytheme("spacelab"),

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
          h2('Upload TLS Data'),
            fileInput('file_TLStest', "Test data", accept = c(".csv", ".txt")), 
            div(style = "margin-top: -20px"),
            textInput(inputId = "dataname_test",
                    label = "Test Data Nickname:",
                    value = "Test"), 
            selectInput('units','TLS units',choices=c('mm', 'meters', 'inches')),
        h3("Historic Comparison"), 
        selectInput('partII_htest', "Test if TLS precision has changed?", choices = c('No', 'Yes')),
        ## Conditional: if the hypothesis test is to be performed, the user now needs to upload more data
        conditionalPanel(
          condition = "input.partII_htest == 'Yes'", 
          fileInput('file_TLSbase', "Historic data", accept = c(".csv", ".txt")), 
          div(style = "margin-top: -20px"),
          textInput(inputId = "dataname_base",
                    label = "Historic Data Nickname:",
                    value = "Historic"),
          selectInput('historic_units','Historic TLS data units',choices=c('mm', 'meters', 'inches'))
        ),
          h2("Upload Reference Lengths"),
            fileInput('filename_tapedata', "Dataset of Reference Lengths", accept = c(".csv", ".txt")), 
            div(style = "margin-top: -25px"),
            selectInput('units_tape','Reference length units',choices=c('mm', 'cm', 'inches')),
            selectInput('report_as', 'Report errors as:', choices = c('Length error (mm)', 'Percentage of reference length')),
            selectInput('includeThreshold', "Set error threshold?", choices = c('No', 'Yes')),
            ## conditional numeric input that appears if 'includeThreshold' == Yes
            conditionalPanel(
              condition = "input.includeThreshold == 'Yes' && input.report_as == 'Length error (mm)'",
              numericInput("threshold_mm","Error threshold (mm)",value = 1, step = 0.1, min = 0.1)
            ),
        conditionalPanel(
          condition = "input.includeThreshold == 'Yes' && input.report_as == 'Percentage of reference length'",
          numericInput("threshold_pct","Error threshold (%)",value = 0.5, step = 0.01, min = 0.01)
        ),
          
          #fluidRow(
          #  column(3,
          #         numericInput("tapeA", label = "Length A", value = 1000, min = 0.01, step = 0.01)),
          #  column(3, 
          #         selectInput("t1_A", label = "Target 1", choices=1:20)), 
          #  column(3, 
          #         selectInput("t2_A", label = "Target 2", choices=1:20, selected = 2)),
          #),
      #), 
          actionButton('runAnalysis','Run Analysis', class = "btn-primary"), 
        ),
        

        # Prepare space for all the output
        mainPanel(
          tabsetPanel(selected = "Part I - Analysis Results", 
                      br(),
                      tabPanel("Information", 
                               uiOutput("info")),
                      ## prepare the output space for "Part I - Analysis" tab 
                      tabPanel("Part I - Analysis Results", 
                               br(),
                          ## If a length error threshold has been included, add the summary statement/table
                          conditionalPanel(condition = "input.includeThreshold == 'Yes'", 
                                                h3("Length Error Summary"), 
                                                uiOutput("length_summary"), 
                                                verbatimTextOutput("lengtherrors_subtable")),
                               br(),
                               ## Part I - plot
                               plotOutput("errorPlot", width = "700px", height = "300px"),
                               br(),
                               ## All the table of errors and statements for Lengths A-F
                               h4("Length A - long horizontal"), 
                                uiOutput("statement_A"),
                                verbatimTextOutput("errorTable_A"),
                               h4("Length B - short horizontal"),
                                uiOutput("statement_B"),
                                verbatimTextOutput("errorTable_B"),
                               h4("Length C - long vertical"), 
                                uiOutput("statement_C"),
                                verbatimTextOutput("errorTable_C"),
                               h4("Length D - short vertical"), 
                                uiOutput("statement_D"),
                                verbatimTextOutput("errorTable_D"),
                               h4("Length E - long diagonal"), 
                                uiOutput("statement_E"),
                                verbatimTextOutput("errorTable_E"),
                               h4("Length F - short diagonal"),
                                uiOutput("statement_F"),
                                verbatimTextOutput("errorTable_F")
                               ),
                      ## prepare the output space for "Part II - Analysis" tab 
                      tabPanel("Part II - Analysis Results",
                            ## if a historic comparison is being made, the first thing to show up should be the 
                            ## results of the statistical hypothesis test
                            conditionalPanel(condition = "input.partII_htest == 'Yes'", 
                                             h2("Statistical Test"), 
                                             uiOutput("p2_interpretation")),
                            ## regardless of historical data, print the standard deviations from the Data Under Test
                               h3("TLS Angular Precision - data under test"),
                               uiOutput("testdata_SDstatements"),
                            ## if making historic comparison, print the same values for the historic data, 
                            ## and then show the data ellipse plot
                            conditionalPanel(condition = "input.partII_htest == 'Yes'", 
                                             h3("Historic values"), 
                                             uiOutput("basedata_SDstatements"), 
                                             br(),
                                             h2("Data Ellipses"), 
                                             plotOutput("ellipsePlot", width = "600px", height = "600px")),

                               h3("Covariance matrices"), 
                               h5("Covariance matrix - data under test"),
                               verbatimTextOutput("SigmaHat2"),
                  
                            conditionalPanel(condition = "input.partII_htest == 'Yes'",
                                           h5("Covariance matix - historic data"),
                                           verbatimTextOutput("SigmaHat1")
                               ),
                            uiOutput("covMat_info")
                      ),
                      ## 
                      ## DATA SUMMARY TAB - report # of Targets/Postions, and plots that check for bad targets
                      ##
                      tabPanel("Data Summary and Report", 
                               h2("Download Report"),
                               downloadButton("download_report", "Generate Report"),
                               h3("Data Summary"),
                               ## data summary statement for TLS data under test
                               textOutput("testdata_summary"), 
                               br(),
                               ## data summary statement for historic TLS data (if applicable)
                               conditionalPanel(condition = "input.partII_htest == 'Yes'", 
                                                textOutput("basedata_summary"),
                                                br()),
                               ## data summary statement for reference lengths (tape measure)
                               textOutput("tapedata_summary"), 
                               ## Now some plots to check for bad targets
                               h3("Check for problem targets"), 
                               uiOutput("residualStatement"),
                               plotOutput("resPlot_test", width = "600px", height = "600px"),
                               conditionalPanel(condition = "input.partII_htest == 'Yes'",
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
server <- function(input, output) {
  
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
    
    ## READ IN THE TLS TEST DATA
    results_out$data2 = data_test()

    ## CONVERT TLS TEST DATA TO mm, IF NECESSARY
    data2_list = convertZc_tomm(data_test(), units = input$units)
    data2 = data2_list$Zc
    data2_targetList = data2_list$target_list
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
      paste("The", dataname_test, "data coordinates were collected in mm.")
    } else {paste("The", dataname_test, "data coordinates were collected in", input$units, "and converted to mm.")
    }
    nTP_test <- TLS_getTandP(data2)
    results_out$testdata_summary <- paste("The", dataname_test, "data are coordinates from", nTP_test$nTargets, "targets 
                                            measured from", nTP_test$nPositions, "TLS positions.", 
                                          TLS_UnitStatement)
    ## 2) Create data summary statement for Tape Measure data
    results_out$tapedata_summary = if(input$units_tape=='mm') {
      paste("Reference lengths were collected in mm. Length errors are in mm")
    } else {paste("Reference lengths were collected in", input$units_tape, "and converted to mm. Length errors are in mm")
    }
    

    ##
    ##**Conditional that historical data is being used:
    ##*  a. create the 'data1' file
    ##*  b. calculate the number of targets/positions
    ##*  c. create the historical data summary statement
    ##*  d. save the data 1 in the 'results_out' stuff
    if(input$partII_htest=="Yes") {
      ## a) create the 'data1' file, converting to mm if necessary
      data1 = convertZc_tomm(data_base(), units = input$units)$Zc
      ## b) determine what the units are
      historicTLS_UnitStatement = if(input$historic_units=='mm') {
        paste("The", dataname_base,  "data coordinates were collected in mm.")
      } else {paste("The", dataname_base, "data coordinates were collected in", input$historic_units, "and converted to mm.")
      }
      ## c) calculate the number of targets/positions
      nTP_base = TLS_getTandP(data1)
      ## d) create/save the data summary statement for the historical TLS data
      results_out$basedata_summary <- paste("The", dataname_base, "data are coordinates from", nTP_base$nTargets, "targets 
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
    incProgress(1/5, detail = "Part I analysis")
    ## CHECK IF TAPE DATA UNITS ARE MM, AND IF NOT CONVERT THEM TO MM
    ## ... 
    ## Calculate the 4 length errors (one per position) for the six lengths

    A_F_LengthError_list = lengthErrors(Dtape = Dtape, Zc = data2, Zc_names = data2_targetList)
    ## Get the table of errors and table of statements
    results_out$A_F_LengthError_table = A_F_LengthError_list$table
    A_F_LengthError_statements = A_F_LengthError_list$statements
    
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
    if(input$includeThreshold=='No') {
      results_out$lengtherrors_summary = "No error threshold was provided."
      results_out$lengtherrors_table = NA
    } else{
      lengtherrors_exceed = lengthError_statement(table = A_F_LengthError_list$table, t_val = results_out$threshold, 
                                                  report_as = results_out$error_reporting)
      results_out$lengtherrors_summary = lengtherrors_exceed$statement
      results_out$lengtherrors_subtable = lengtherrors_exceed$table
    }
    
    
    
    ##*********************
    ## PART II CALCULATIONS
    ##*********************
    ## Step 1:
    ## transform the Cartesian coordinates into spherical residuals
    incProgress(2/5, detail = "rigid body transformation on test data")
    results_out$R2 = Zc_to_R(data2)
    
    ## Step 2:
    ## Transform the residuals from wide to long
    results_out$X2 = RtoX(results_out$R2)
    
    ## CALCULATE THE COVARIANCE MATRIX FROM THE DATA UNDER TEST
    results_out$SigmaHat2 = cov(results_out$X2)[1:2,1:2]

    ## create the statements about the standard deviations in the angular/ranging residuals.
    testdata_sd = round(sqrt(diag(round(results_out$SigmaHat2,3))),2)
    results_out$testdata_SDstatements = HTML(
      paste("The standard deviation in the azimuth angle residuals is", testdata_sd[1], "arcsec.<br>", 
            "The standard deviation in the elevation angle residuals is", testdata_sd[2], "arcsec."))
            #"The standard deviation in the ranging residuals is", testdata_sd[3], "mm."))
    
    ## CHECK FOR BAD TARGETS -- punt the 'R_pretty' dataset for the Historic dataset, if applicable
    results_out$Rpretty_test <- create_pretty_R(results_out$R2, data_name = dataname_test)
    
    ##**CONDITIONAL ON WHETHER HISTORICAL COMPARISON IS BEING DONE**
    ##* Do the same steps on the historical ('baseline') data
    if(input$partII_htest=="Yes") {
      incProgress(3/5, detail = "rigid body transformation on historic data")
      ## Transform Cartesian coordinates to spherical residuals
      results_out$R1 = Zc_to_R(data1)
      ## transform from wide to long
      results_out$X1 = RtoX(results_out$R1)
      ## estimate the covariance matrix
      results_out$SigmaHat1 = cov(results_out$X1)[1:2,1:2]
      
      ## get statement on the historic standard deviations
      basedata_sd = round(sqrt(diag(round(results_out$SigmaHat1,3))),2)
      results_out$basedata_SDstatements = HTML(
        paste("The historical standard deviation in the azimuth angle residuals is", basedata_sd[1], "arcsec.<br>", 
              "The historical standard deviation in the elevation angle residuals is", basedata_sd[2], "arcsec.")) 
              #"The historical standard deviation in the ranging residuals is", basedata_sd[3], "mm."))
      
      ## Step 3:
      ## Perform the hypothesis test
      ## Step 3: test equality of the covariance matrices
      incProgress(4/5, detail = "Part II analysis")
      p2_results = TLS_cov_check(results_out$X1[,1:2], results_out$X2[,1:2], conf.level = .99)
      results_out$test_results = p2_results$results
      results_out$p2_conclusion = p2_results$conclusion
      results_out$p2_interpretation = p2_results$interpretation
    
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
      results_out$Xcomb = Xcomb[,-3]
      
      ## CHECK FOR BAD TARGETS -- punt the 'R_pretty' dataset for the historica data, if applicable 
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
  output$lengtherrors_subtable <- renderPrint({
    if(is.null(all_results()$lengtherrors_subtable) | nrow(all_results()$lengtherrors_subtable)==0) {
      invisible()} else{
    return(all_results()$lengtherrors_subtable)
      }
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
      plot_as = all_results()$error_reporting
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
                 "phi = elevation angle (arcsec)"
                 #"r = ranging direction (mm)"
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
    return(all_results()$p2_interpretation)
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
          length_summary = all_results()$lengtherrors_summary,
          error_plot = error_plot_reactive() # The new plot reactive
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
