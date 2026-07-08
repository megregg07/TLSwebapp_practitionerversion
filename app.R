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

          width = 4, # Optional: Increasing sidebar width slightly can help side-by-side layouts
          
          ## 1. SELECT THE ASSESSMENT TYPE (BASELINE OR VERIFICATION)
          h3("Assessment Type"), 
          selectInput('assessment_type', "What type of assessment is being performed?", 
                      choices = c('Choose an option...' = '',
                                  'Baseline Assessment', 
                                  'Verification Assessment')), 
          
          ## 
          ## 2. UPLOAD THE CURRENT DATA 
          ##    1a. TLS data
          ##    1b. Reference lengths
          ##
          conditionalPanel(
            condition = "input.assessment_type != ''",
            hr(style = "border-top: 1px solid #aaaaaa;"),
            fluidRow(
              
              ## DYNAMIC HEADER BASED ON ASSESSMENT TYPE SELECTION
              conditionalPanel(
                condition = "input.assessment_type == 'Baseline Assessment'",
                h3("1. Upload Baseline Data")
              ), 
              conditionalPanel(
                condition = "input.assessment_type == 'Verification Assessment'",
                h3("1. Upload Verification Data")
              ),
              
              # --- Column 1: TLS Data ---
              column(6, 
                    fileInput('file_TLStest', "1a. TLS Data", accept = c(".csv", ".txt")), 
                    div(style = "margin-top: -20px"),
                    selectInput('units', 'TLS units', choices = c('mm', 'meters', 'inches')),
                    textInput(inputId = "dataname_test",
                               label = "Data Nickname:",
                               value = "Test data")
              ),
            
              # --- Column 2: Reference Lengths ---
              column(6, 
                    #h3("1b. Upload reference length data"),
                    fileInput('filename_tapedata', "1b. Reference Length Data", accept = c(".csv", ".txt")), 
                    div(style = "margin-top: -20px"),
                    selectInput('units_tape', 'Reference length units', choices = c('mm', 'cm', 'inches'))
              )
            )
          ),
          
          ## 3. UPLOAD THE HISTORIC DATA
          ##    (only appears if "Verification Assessment" is selected)
          conditionalPanel(
            condition = "input.assessment_type == 'Verification Assessment'",
            ## add a faint line for visual distinction
            hr(style = "border-top: 1px solid #aaaaaa;"),
          
            ## 
            ## 2. ARE WE MAKING A HISTORICAL COMPARISON?
            ##    If yes:
            ##      2a. TLS data
            ##      2b. Reference lengths
            ##
          h3("2. Upload Baseline Data"), 
          # selectInput('partII_htest', "Compare the current data to a previous set of data?", choices = c('No', 'Yes')),
          ## If yes, allow user to upload data
          #conditionalPanel(
          #  condition = "input.partII_htest == 'Yes'", 
            fluidRow(
              # --- Column 1: historic TLS Data ---
              column(6, 
                     #h3("1a. Upload TLS Data"),
                     fileInput('file_TLSbase', "2a. Baseline TLS Data", accept = c(".csv", ".txt")),
                     div(style = "margin-top: -20px"),
                     selectInput('historic_units','Baseline TLS data units',choices=c('mm', 'meters', 'inches')),
                     textInput(inputId = "dataname_base",
                               label = "Baseline Data Nickname:",
                               value = "Baseline data")
              ),
              #--- Column 2: Historic Reference Lengths ---
              column(6, 
                     fileInput('filename_reflengths_hist', "2b. Baseline Reference Length Data", accept = c(".csv", ".txt")), 
                     div(style = "margin-top: -20px"),
                     selectInput('reflengths_hist_units', 'Baseline reference length units', choices = c('mm', 'cm', 'inches'))
              )
            )
          ), # <-- CLOSES CONDITIONAL PANEL
          ## add a faint line for visual distinction
          hr(style = "border-top: 1px solid #aaaaaa;"),
          
          ##
          ## 3. HOW DO YOU WANT THE PART I ERRORS TO BE REPORTED?
          ##
          conditionalPanel(
            condition = "input.assessment_type != ''", 
            
            ## DYNAMIC HEADER BASED ON ASSESSMENT TYPE SELECTION
            conditionalPanel(
              condition = "input.assessment_type == 'Baseline Assessment'",
              h3("2. Error Reporting and MPE")
            ), 
            conditionalPanel(
              condition = "input.assessment_type == 'Verification Assessment'",
              h3("3. Error Reporting and MPE")
            ),
            
            ##**GO BACK TO THIS WHEN ADDING BACK IN THE FEATURE OF SETTING MPE AS A PERCENTAGE**
            #selectInput('report_as', 'Report errors as:', choices = c('Length error (mm)', 'Percentage of reference length')),
            # --- TEMPORARY SELECTION: SHOW THAT % WILL BE AN OPTION IN THE FUTURE ---
            tags$div(class = "form-group shiny-input-container",
                     tags$label(class = "control-label", `for` = "report_as", "Report errors as:"),
                     tags$select(id = "report_as", class = "form-control",
                                 tags$option(value = "Length error (mm)", "Length error (mm)"),
                                 tags$option(value = "Percentage of reference length (forthcoming feature)", 
                                             "Percentage of reference length (forthcoming feature)", disabled = "disabled")
                     )
            ),

          
            ## MPE STUFF IF USER WANTS ERRORS REPORTED IN MM
            conditionalPanel(
              condition = "input.report_as == 'Length error (mm)'",
              ## MPE input (in mm)
              numericInput("threshold_mm","Maximum Permissible Error Value (mm):",value = 5, step = 0.1, min = 1), 
              # Reference Length Uncertainty Input (Defaults to 1mm)
              numericInput("unc_ref", "Reference Length Uncertainty (mm):", 
                           value = 1, step = 0.1, min = 0.01),
              # Placeholder for making uncertainty value too low Warning
              uiOutput("unc_warning"),
              ## Placeholder for the 4:1 Warning
              uiOutput("mpe_ratio_warning")
            ),
            
            ## MPE INPUTS IF USER WANTS ERRORS REPORTED AS PERCENTAGE 
            conditionalPanel(
              condition = "input.report_as == 'Percentage of reference length'",
              numericInput("threshold_pct","Maximum Permissible Error Value (%):",value = 0.5, step = 0.01, min = 0.01)
            )
          ), # <--- THIS CLOSES THE NEW ASSESSMENT TYPE CONDITIONAL PANEL
          
          ## WHERE ERROR MESSAGE RELATING TO DUPLICATE FILES IS DISPLAYED
          uiOutput("file_validation_display"),
        
        fluidRow(
          column(width = 6, 
                 shinyjs::disabled(actionButton('runAnalysis','Run Analysis', class = "btn-primary"))
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
              h4("Advanced settings"),
              numericInput("alpha", "Hypothesis test significance threshold", value = 0.01, step = 0.001, min = .001),
              numericInput("seed_value", "MCS seed value", value = rand_seed), 
              numericInput("nIt_mcs", "Number of MCS iterations", value = 3000, min = 500)
          )
        )
        ), # <--- THIS PARENTHESIS CLOSES THE SIDEBAR PANEL CORRECTLY

        # Prepare space for all the output
        mainPanel(
          width = 8, ## set the width = 8, so the total width of the sidebar + mainpanel is equal to 12
          tabsetPanel(
            id = 'results_tabs',
            selected = "Part I - Accuracy", 
                      br(),
                      tabPanel("Information", 
                               uiOutput("info")),
                      ## prepare the output space for "Part I - Analysis" tab 
                      tabPanel("Part I - Accuracy", 
                               ## COMPLIANCE SUMMARY
                               uiOutput("partI_summary_box"),
                               br(),
                               
                               ## DYNAMIC MAIN HEADERS
                               conditionalPanel(
                                 condition = "input.assessment_type == 'Baseline Assessment'",
                                 h2("Accuracy - Baseline Data")
                               ),
                               conditionalPanel(
                                 condition = "input.assessment_type == 'Verification Assessment'",
                                 h2("Accuracy - Verification Data")
                               ),
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
                                br(), 
                               ## IF HISTORIC COMPARISON IS MADE, REPORT VALUES HERE
                               conditionalPanel(
                                 condition = "input.assessment_type =='Verification Assessment'",
                                 h2("Baseline Accuracy"),
                                 ## Historical Part I accuracy statement
                                 uiOutput("length_summary_hist"), 
                                 ## Summary table -- if threshold given and no values exceed, then there will be no table 
                                 uiOutput("lengtherrors_subtable_hist"),
                                 br(),
                                 ## Historical Part I accuracy plot
                                 plotOutput("errorPlot_hist", width = "700px", height = "350px"),
                                 br(),
                               )
                                #verbatimTextOutput("errorTable_F")
                               ),
                      ## prepare the output space for "Part II - Analysis" tab 
                      #tabPanel("Part II - Precision",
                            ## if a historic comparison is being made, the first thing to show up should be the 
                            ## results of the statistical hypothesis test
                      #        h2("Precision Summary"),
                            ## regardless of historical data, print the standard deviations from the Data Under Test
                               #h3("TLS Angular Precision - data under test"),
                               #uiOutput("testdata_SDstatements")

                              ##**COVARIANCE MATRICES CAN BE PUT IN THE REPORT*
                               #h3("Covariance matrices"), 
                               #h5("Covariance matrix - data under test"),
                               #verbatimTextOutput("SigmaHat2"),
                  
                            #conditionalPanel(condition = "input.partII_htest == 'Yes'",
                            #               h5("Covariance matix - historic data"),
                            #               verbatimTextOutput("SigmaHat1")
                            #   ),
                            #uiOutput("covMat_info")
                      #),
                      ## 
                      ## Historic comparison Will appear here by Server code
                      ##
                      tabPanel("Part II - Precision", 
                               ## COMPLIANCE SUMMARY BOX
                               ## Universal Part II Compliance Summary Box
                               uiOutput("partII_summary_box"),
                               br(),
                               
                               ## DYNAMIC MAIN HEADERS
                               conditionalPanel(
                                 condition = "input.assessment_type == 'Baseline Assessment'",
                                 h2("Precision - Baseline Data")
                               ),
                               conditionalPanel(
                                 condition = "input.assessment_type == 'Verification Assessment'",
                                 h2("Precision - Verification Data")
                               ),
                               
                               uiOutput("testdata_SDstatements"), 
                               br(),
                               
                               # Everything inside this panel only appears if 'Yes' is selected
                               conditionalPanel(
                                 condition = "input.assessment_type == 'Verification Assessment'",
                                 ## report SDs from Historic data
                                 h2("Precision - Baseline Data"), 
                                 uiOutput("basedata_SDstatements"),
                                 h2("Precision Comparison"),
                                 ## Report the results of the hypothesis test
                                 uiOutput("p2_interpretation"), 
                                 br(),
                                 ## data ellipse plot
                                 uiOutput("ellipse_container"), 
                                 ##
                                 ## IN CASE THE HYPOTEHSIS TEST REJECTS H0...
                                 ## statement about the expected error range from MCS
                                 uiOutput("MCS_results_container"),
                                 #uiOutput("EER_explanation"),
                                 #br(),
                                 ## ** put the MCS results here
                                 #uiOutput("expectederrors_summary"), 
                                 #verbatimTextOutput("expectederrors_subtable"),
                                 #uiOutput("expectederrors_subtable"),
                                 #br(),
                                 #plotOutput("expectederrorPlot", width = "700px", height = "350px")
                               ),
                               ),

            ## 
            ## DATA SUMMARY & REPORT TAB
            ## 
            tabPanel("Summary & Report", 
                     ## box that will print the overall assessment conclusion
                     # ---- PASTE THE SUMMARY BOX HERE ----
                     uiOutput("overall_assessment_summary"),
                     hr(style = "border-top: 1px solid #aaaaaa;"),
                     
                     # 1. Main horizontal flex container holding both columns side-by-side
                     div(style = "display: flex; justify-content: flex-start; gap: 50px;",
                         
                         # --- Column 1: Primary Data Metadata ---
                         div(style = "min-width: 250px;", 
                             conditionalPanel(
                               condition = "input.assessment_type == 'Baseline Assessment'",
                               HTML("<h3>Baseline Data Metadata:</h3>")
                             ),
                             conditionalPanel(
                               condition = "input.assessment_type == 'Verification Assessment'",
                               HTML("<h3>Verification Data Metadata:</h3>")
                             ),
                             uiOutput("summary_currentdata"),
                             br(),
                             uiOutput("resPlot_container_test")
                         ), # <-- Closes Column 1 div
                         
                         # --- Column 2: Baseline Comparison Data (Verification track only) ---
                         conditionalPanel(
                           condition = "input.assessment_type == 'Verification Assessment'",
                           div(style = "min-width: 250px;", 
                               HTML("<h3>Baseline Data Metadata:</h3>"), 
                               uiOutput("summary_historicdata"),
                               br(),
                               uiOutput("resPlot_container_base")
                           ) # <-- Closes Column 2 inner div
                         ) # <-- Closes Column 2 conditionalPanel
                         
                     ), # <-- Closes main horizontal flex container div
                     
                     br(),
                     
                     # 2. Optional Report Notes Section
                     div(style = "max-width: 600px;", 
                         textAreaInput(inputId = "report_notes", 
                                       label = "Report Notes (Optional):", 
                                       placeholder = "Enter details for your records", 
                                       rows = 4, 
                                       width = "100%")
                     ), # <-- Closes notes div
                     
                     br(),
                     
                     # 3. Action Buttons Section (Generate Report & Reset)
                     div(style = "display: flex; justify-content: flex-start; gap: 50px;", 
                         div(style = "min-width: 250px;",
                             downloadButton("download_report", "Generate Report")
                         ),
                         div(style = "min-width: 250px;", 
                             actionButton("reset_button", "Reset App", icon = icon("undo"), class = "btn-danger")
                         )
                     )
            ) # <--- CLOSES METADATA & REPORT TAB PANEL CLEANLY
          )
        ) # <--- MAIN PANEL ENDS
    ) # <--- SIDEBAR LAYOUT ENDS
) # <--- FLUID PAGE ENDS


##############################
##
## DEFINE THE SERVER LOGIC
##
##############################
server <- function(input, output, session) {
  
 
  
  # Dynamically change data nicknames based on assessment type
  observeEvent(input$assessment_type, {
    if (input$assessment_type == "Baseline Assessment") {
      updateTextInput(session, "dataname_test", value = "Baseline data")
      updateTextInput(session, "dataname_base", value = "") # Clear out historic if baseline
    } else if (input$assessment_type == "Verification Assessment") {
      updateTextInput(session, "dataname_test", value = "Verification data")
      updateTextInput(session, "dataname_base", value = "Baseline data")
    }
  })
  
  ## WARNING IF REFERENCE LENGTH UNCERTAINTY IS BELOW THE ESTABILSHED VALUE
  output$unc_warning <- renderUI({
    req(input$unc_ref)
    
    if (input$unc_ref < 1) {
      div(style = "color: #8a6d3b; background-color: #fcf8e3; border: 1px solid #faebcc; padding: 12px; margin-top: 15px; border-radius: 4px; font-size: 0.9em;",
          icon("exclamation-triangle"),
          strong(" Documentation Required:"), 
          "You will need formal documentation to support using a reference length uncertainty value this low."
      )
    }
  })
  
  # ------------------------------------------------------------------
  # WARNING: CHECK THE 4:1 RATIO
  # ------------------------------------------------------------------
  output$mpe_ratio_warning <- renderUI({
    req(input$threshold_mm, input$unc_ref)  # <-- Updated to input$threshold_mm
    
    min_allowable_mpe <- 4 * input$unc_ref
    if (input$threshold_mm < min_allowable_mpe) {  # <-- Updated to input$threshold_mm
      div(style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; padding: 12px; margin-top: 10px; border-radius: 4px; font-size: 0.9em;",
          icon("ban"),
          strong(" Ratio Violation:"), 
          sprintf(" The MPE must be at least 4 times the reference length uncertainty. Minimum MPE allowed for the current uncertainty value is %s mm.", min_allowable_mpe)
      )
    }
  })
  # ------------------------------------------------------------------
  # LOCK 'RUN ANALYSIS' BUTTON IF 4:1 RULE VIOLATED
  # ------------------------------------------------------------------
  observe({
    req(input$threshold_mm, input$unc_ref)  # <-- Updated to input$threshold_mm
    
    # Check if the 4:1 rule is violated using the correct UI ID
    if (input$threshold_mm < (4 * input$unc_ref)) {  # <-- Updated to input$threshold_mm
      shinyjs::disable("runAnalysis")
    } else {
      shinyjs::enable("runAnalysis")
    }
  })
  

  ## OBSERVE IF WE NEED THE ADVANCED SETTINGS OPTION
  observeEvent(input$toggle_adv, {
    toggle("advanced_section", anim = TRUE)
  })
  
  ## OBSERVE IF THE USER WANTS TO RESET EVERYTING
  observeEvent(input$reset_button, {
    session$reload()
  })
  
  ## FORCE DATA TO BE UPLOADED BEFORE 'RUN ANALYSIS' CAN BE CLICKED 
  observe({
    # 1. Check if primary files are uploaded
    has_primary <- !is.null(input$file_TLStest) && !is.null(input$filename_tapedata)
    
    # 2. Check if comparison files are uploaded
    has_baseline_comp <- !is.null(input$file_TLSbase) && !is.null(input$filename_reflengths_hist)
    
    # 3. Determine if current conditions meet the criteria for the chosen path
    requirements_met <- if (input$assessment_type == "Baseline Assessment") {
      has_primary
    } else if (input$assessment_type == "Verification Assessment") {
      has_primary && has_baseline_comp
    } else {
      FALSE # If no assessment type is chosen yet, keep it disabled
    }
    
    # 4. Use shinyjs to instantly flip the button state in the user's browser
    shinyjs::toggleState("runAnalysis", condition = requirements_met)
  })
  
  # AUTOMATICALLY JUMP TO THE 'PART I' TAB 
  ## (prevents the analyses from being stalled b/c user was on INFO tab)
  observeEvent(input$runAnalysis, {
    updateTabsetPanel(
      session = session, 
      inputId = "results_tabs",          # The ID of your tabsetPanel
      selected = "Part I - Accuracy"     # The exact title of the tab you want to show
    )
  })
  
  ##**************************
  # --- CENTRAL VALIDATION ---
  # This checks all file relationships before any data is read
  file_error_message <- reactive({
    # Core requirement: The primary data inputs must exist to check anything
    if (is.null(input$file_TLStest) || is.null(input$filename_tapedata)) return(NULL)
    
    # Get labels based on assessment track for dynamic error text
    is_verification <- (input$assessment_type == "Verification Assessment")
    primary_label <- if (is_verification) "Verification" else "Baseline"
    
    # 1. ALWAYS check Primary TLS vs Primary Reference Lengths
    if (input$file_TLStest$name == input$filename_tapedata$name) {
      return(paste("Error:", primary_label, "TLS Data and", primary_label, "Reference Lengths must be different files."))
    }
    
    # Logic for Baseline Comparison Data (Only checked during Verification tracks)
    if (is_verification && !is.null(input$file_TLSbase)) {
      # 2. Check Verification TLS vs Baseline TLS
      if (input$file_TLStest$name == input$file_TLSbase$name) {
        return("Error: Verification TLS Data and Baseline TLS Data must be different files.")
      }
      # 3. Check Baseline TLS vs Verification Reference Lengths
      if (input$file_TLSbase$name == input$filename_tapedata$name) {
        return("Error: Baseline TLS Data and Verification Reference Lengths must be different files.")
      }
    }
    
    # Logic for Baseline Reference Lengths
    if (is_verification && !is.null(input$filename_reflengths_hist)) {
      # 5. Check Verification TLS vs Baseline Reference Lengths
      if (input$file_TLStest$name == input$filename_reflengths_hist$name) {
        return("Error: Verification TLS Data and Baseline Reference Lengths must be different files.")
      }
      # 6. Check Verification Reference Lengths vs Baseline Reference Lengths
      if (input$filename_tapedata$name == input$filename_reflengths_hist$name) {
        return("Error: Verification Reference Lengths and Baseline Reference Lengths must be different files.")
      }
      
      # 4. Check Baseline TLS vs Baseline Reference Lengths
      if (!is.null(input$file_TLSbase)) {
        if (input$file_TLSbase$name == input$filename_reflengths_hist$name) {
          return("Error: Baseline TLS Data and Baseline Reference Lengths must be different files.")
        }
      }
    }
    
    return(NULL) # No duplicates found
  })
  
  ##################################################
  ##**CHECK THAT THE USER HAS UPLOADED UNIQUE FILES*
  output$file_validation_display <- renderUI({
    msg <- file_error_message()
    req(msg) # Only run if there is actually a message
    
    # Display a single, clean red error box
    div(style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; 
               padding: 15px; border-radius: 4px; margin-bottom: 20px; font-weight: bold;",
        icon("exclamation-circle"), " ", msg)
  })
  
  ###########################################################
  ##***IMPORT THE DATA***
  ##
  ##***Current data**
  ## IMPORT THE CURRENT TLS DATA (the data under test)
  data_test = reactive({
    # This stops the reactive silently if there is a file error
    req(is.null(file_error_message()), cancelOutput = TRUE)
    
    if(is.null(input$file_TLStest)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$file_TLStest$name)
      data_test = switch(ext, csv = read.csv(input$file_TLStest$datapath), 
                           txt = read.table(input$file_TLStest$datapath, header = TRUE), 
                           validate("Invalid file; Please upload a .csv or .txt file"))
      return(data_test)
    }
  })
  ## IMPORT THE CURRENT REFERENCE VALUES ('tape data')
  Dtape = reactive({
    # This stops the reactive silently if there is a file error
    req(is.null(file_error_message()), cancelOutput = TRUE)
    
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
  ##**BASELINE data**
  ##*
  ## IMPORT THE HISTORIC TLS DATA (baseline)
  data_base <- reactive({
    # Only run if the file exists 
    req(input$file_TLSbase)
    # This stops the reactive silently if there is a file error
    req(is.null(file_error_message()), cancelOutput = TRUE)
    
    ext <- tools::file_ext(input$file_TLSbase$name)
    switch(ext, 
           csv = read.csv(input$file_TLSbase$datapath), 
           txt = read.table(input$file_TLSbase$datapath, header = TRUE), 
           validate("Invalid file; Please upload a .csv or .txt file"))
  })
  
  ## IMPORT THE HISTORIC REFERENCE VALUES ('tape data)
  reflengths_hist = reactive({
    req(input$filename_reflengths_hist)
    # This stops the reactive silently if there is a file error
    req(is.null(file_error_message()), cancelOutput = TRUE)

    ext = tools::file_ext(input$filename_reflengths_hist$name)
    reflengths_hist = switch(ext, csv = read.csv(input$filename_reflengths_hist$datapath), 
                    txt = read.table(input$filename_reflengths_hist$datapath, header = TRUE), 
                    validate("Invalid file; Please upload a .csv or .txt file"))
    return(reflengths_hist)

  })

  
  ## 
  ## SOMETHING THAT WILL CHECK WHETHER THE USER WANTS TO SEE THE 'DATA ELLIPSE' PLOT (IF APPLICABLE)
  show_ellipse <- reactiveVal(FALSE)
  # Every time "Run Analysis" is clicked, hide the plot link again
  observeEvent(input$runAnalysis, {show_ellipse(FALSE)  })
  # Listens for the link click (outside the main analysis)
  observeEvent(input$view_plot_link, {show_ellipse(TRUE)})
  ## Link to hide the plot
  observeEvent(input$hide_plot_link, {show_ellipse(FALSE)})
  
  ## 
  ## SOMETHING THAT WILL CHECK WHETHER THE USER WANTS TO SEE THE RESIDUAL PLOT (CURRENT DATA)
  # --- Reactive Value ---
  show_rp_current <- reactiveVal(FALSE)
  # --- Observers ---
  observeEvent(input$runAnalysis, { show_rp_current(FALSE) })
  observeEvent(input$view_rp_current, { show_rp_current(TRUE) })
  observeEvent(input$hide_rp_current, { show_rp_current(FALSE) })
  
  ## 
  ## CHECK WHETHER THE USER WANTS TO SEE THE RESIDUAL PLOT (HISTORIC DATA)
  # --- Reactive Value ---
  show_rp_historic <- reactiveVal(FALSE)
  # --- Observers ---
  observeEvent(input$runAnalysis, { show_rp_historic(FALSE) })
  observeEvent(input$view_rp_historic, { show_rp_historic(TRUE) })
  observeEvent(input$hide_rp_historic, { show_rp_historic(FALSE) })
  
  

  ## 
  ## NOW DO STUFF, WHEN THE BUTTON IS CLICKED
  ##
  all_results <- eventReactive(input$runAnalysis, {
    
    ##**CHECK IF DATA FILES WERE UPLOADED**
    ##*
    ##*# NEW GUARD CLAUSE: Explicitly intercept missing files before running calculations
    if (input$assessment_type == "Verification Assessment") {
      validate(
        need(input$file_TLStest, "Missing File: Please upload the Verification TLS data."),
        need(input$filename_tapedata, "Missing File: Please upload the Verification reference lengths."),
        need(input$file_TLSbase, "Missing File: Please upload the Baseline TLS data comparison file."),
        need(input$filename_reflengths_hist, "Missing File: Please upload the Baseline reference lengths comparison file.")
      )
    } else if (input$assessment_type == "Baseline Assessment") {
      validate(
        need(input$file_TLStest, "Missing File: Please upload the Baseline TLS data."),
        need(input$filename_tapedata, "Missing File: Please upload the Baseline reference lengths.")
      )
    }
    
    # Ensure no duplicate file validations are failing
    validate(need(is.null(file_error_message()), "Please resolve file duplication errors before running analysis."))
    
    # Verify that a numeric threshold value is safely provided
    if (input$report_as == 'Length error (mm)') {
      validate(need(input$threshold_mm, "Please enter a valid numeric value for the Error Specification (mm)."))
    } else {
      validate(need(input$threshold_pct, "Please enter a valid numeric value for the Error Specification (%)."))
    }
    
    ## ESTABLISH PROGRESS BAR
   withProgress(message = 'Performing', value = 0, {
    
    results_out = list()
    
    
    ## USE THIS TO DEBUG
    #browser()
    
    ## DETERMINE WHETHER A HISTORIC COMPARISON IS BEING MADE
    results_out$partII_htest <- if(input$assessment_type == "Verification Assessment") "Yes" else "No"
    
    ## READ IN THE ADVANCED SETTINGS PARAMETERS**
    results_out$alpha <- input$alpha
    results_out$seed_value <- input$seed_value
    results_out$nIt_mcs <- input$nIt_mcs
    
    ## GET THE FILE NAMES 
    results_out$filename_TLStest <- input$file_TLStest$name  ## file name for current TLS data
    results_out$filename_reflengths <- input$filename_tapedata$name ## filename for current reference lengths
    
    ## GET THE (possibly user-defined) NICKNAMES FOR THE DATASETS
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
    
    ## READ IN THE TAPE MEASURE DATA
    Dtape = Dtape()
    ## If tape_units are not already mm, convert to mm (unit options are mm, cm or inches)
    Dtape = Dtape_ToMM(dat = Dtape, units = input$units_tape)
    
    ##
    ##**DETERMINE IF AN ERROR THRESHOLD WAS SET*
    ##
    ## DETERMINE IF AN ERROR THRESHOLD WAS SET...this could either be given in mm or as a %
    ## There is no longer a need to convert the threshold to mm...it must be given as mm or %
    ## first, convert threshold to mm, if necessary
    ## if includeThreshold==No, make NA,
    ## otherwise, need to determine the units_tape and see if the threshold needs to change
    results_out$threshold <- switch(input$report_as, 
                                    'Length error (mm)' = input$threshold_mm, 
                                    'Percentage of reference length' = input$threshold_pct
    )
    
    ## this can be fed directly into the Part I plotting function
    results_out$error_reporting = switch(input$report_as, 
                                         'Length error (mm)' = 'mm', 
                                         'Percentage of reference length' = 'pct')
    
    ##
    ##**OBTAIN THE REFERENCE LENGTH UNCERTAINTY VALUE** (and warning, if necessary)
    results_out$unc_ref = input$unc_ref

    ##**************************
    ## DATA SUMMARY STATEMENTS
    ##
    # GET UNITS FOR TLS DATA UNDER TEST AND TAPE
    results_out$units = input$units
    results_out$units_tape = input$units_tape
    
    ## 1) Create data summary statement for TLS data under test
    results_out$TLS_UnitStatement = if(input$units=='mm') {
      paste("The", paste0('"',dataname_test,'"'), "coordinates were collected in mm.")
    } else {paste("The", paste0('"',dataname_test,'"'), "coordinates were collected in", input$units, "and converted to mm.")
    }
    nTP_test <- TLS_getTandP(data2)
    ## save the number of targets/positions for the TLS data under test
    results_out$nT_test <- nTP_test$nTargets
    results_out$nP_test <- nTP_test$nPositions
    results_out$testdata_summary <- paste("The", paste0('"',dataname_test,'"'), "data are Cartesian coordinates from", nTP_test$nTargets, "targets 
                                            measured from", nTP_test$nPositions, "TLS positions.", 
                                          results_out$TLS_UnitStatement)
    ## 2) Create data summary statement for Tape Measure data
    results_out$tapedata_summary = if(input$units_tape=='mm') {
      paste("Reference lengths were collected in mm.")
    } else {paste("Reference lengths were collected in", input$units_tape, "and converted to mm.")
    }
    

    ##
    ##**Conditional that historical data is being used:
    ##*  a. create the 'data1' file
    ##*  b. calculate the number of targets/positions
    ##*  c. create the historical data summary statement
    ##*  d. save the data 1 in the 'results_out' stuff
    if(input$assessment_type == "Verification Assessment") {
      ## 1. GET THE FILE NAMES 
      results_out$filename_TLShist <- input$file_TLSbase$name  ## file name for historic TLS data
      results_out$filename_reflengths_hist <- input$filename_reflengths_hist$name ## filename for historic reference lengths
      
      ## 1. Read in the historic TLS data and do stuff with it
      ##    a) create the 'data1' file, converting to mm if necessary
      Zc1_named = convertZc_tomm(data_base(), units = input$historic_units)
      data1 = Zc1_named[,-1]
      ##    aa) separate out the list of target names for the historic data
      data1_targetList = Zc1_named[,1] 
      ##    b) determine what the units are
      results_out$historicTLS_UnitStatement = if(input$historic_units=='mm') {
        paste("The", paste0('"',dataname_base,'"'),  "coordinates were collected in mm.")
      } else {paste("The", paste0('"',dataname_base,'"'), "coordinates were collected in", input$historic_units, "and converted to mm.")
      }
      ##    c) calculate the number of targets/positions
      nTP_base = TLS_getTandP(data1)
      results_out$nT_base = nTP_base$nTargets
      results_out$nP_base = nTP_base$nPositions
      ##    d) create/save the data summary statement for the historical TLS data
      results_out$basedata_summary <- paste("The", paste0('"',dataname_base,'"'), "data are Cartesian coordinates from", nTP_base$nTargets, "targets 
                                            measured from", nTP_base$nPositions, "TLS positions.", 
                                            results_out$historicTLS_UnitStatement)
      ##    e) save the data 1 in the 'results_out' collection
      results_out$data1 = data1

      
      ##
      ## 2. Read in the historic reference length data and do stuff with it
      ##    a) Read in the historic Reference Length data
      reflengths_hist = reflengths_hist()
      ##    b) Read in the units for this data
      results_out$reflengths_hist_units = input$reflengths_hist_units
      ## Add a nice statement about the historic reference data units (were they converted?)
      results_out$reflengths_hist_summary = if(input$reflengths_hist_units=='mm') {
        paste("Baseline reference lengths were collected in mm.")
      } else {paste("Baseline reference lengths were collected in", input$units_tape, "and converted to mm.")
      }
      
      ##    c) Convert the historic reference lengths to mm, if necessary
      reflengths_hist = Dtape_ToMM(dat = reflengths_hist, units = input$reflengths_hist_units)
      ##    d) Calculate the length errors for the historic data
      A_F_LengthError_list_hist = lengthErrors(Dtape = reflengths_hist, Zc = data1, Zc_names = data1_targetList)
      results_out$A_F_LengthError_table_hist = A_F_LengthError_list_hist$table
      ##    e) Do all the stuff to calculate/report Part I-type reporting but for the historic data
      results_out$A_F_lengthError_statements_hist = A_F_LengthError_list_hist$statements
      ##
      ## If a threshold was included, count how many absolute length errors are larger
      ## than the threshold, and report the values that were, if applicable
      ## 
      if(FALSE) {  ## I removed the includeThreshold option (the user must now always have a threshold value)
        
      #if(input$includeThreshold=='No') {
        ## IF NO THRESHOLD IS PROVIDED, PROVIDE A TABLE OF THE LARGEST THREE ERRORS
        ## the Statement and the table will change, depending on whether the user wants the results
        ## reported as length errors or as % error...
        results_out$lengtherrors_summary_hist = switch(input$report_as, 
                                                  'Length error (mm)' = "No error specification was provided. Displaying the three largest historic length errors.",
                                                  'Percentage of reference length' = "No error specification was provided. Displaying the three largest historic percent errors"
        )
        results_out$lengtherrors_subtable_hist = switch(input$report_as,
                                                   'Length error (mm)' = (results_out$A_F_LengthError_table_hist %>% arrange(desc(abs(Error))))[1:3,], 
                                                   'Percentage of reference length' = (results_out$A_F_LengthError_table_hist %>% arrange(desc(abs(PctError))))[1:3,]                                    
        )
        results_out$lengtherrors_statement_hist_clean = results_out$lengtherrors_summary_hist
      } else{
        ## First make a generic version of the lengthError table so I can feed it into my 'statements' function
        gtab_PartI_hist <- results_out$A_F_LengthError_table_hist %>% 
          reframe(ID, ReferenceLength, Position, Length, 
                  y = Error, y_pct = PctError)
        ## use the 'statements' function on the generic table
        ##**PROBABLY NEED TO EDIT THE 'lengthError_statement' function to either be 'historic' statement or 'current' statement*
        lengtherrors_exceed_hist = lengthError_statement(table = gtab_PartI_hist, t_val = results_out$threshold, 
                                                    report_as = results_out$error_reporting, section = "PartI")
        results_out$lengtherrors_summary_hist = paste("<b>Baseline data:</b>", lengtherrors_exceed_hist$statement)
        results_out$lengtherrors_subtable_hist = lengtherrors_exceed_hist$table
        ## make a clean statement for punting to the PDF document
        results_out$lengtherrors_statement_hist_clean = lengtherrors_exceed_hist$statement
      }
      
      }

    #browser()


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

    if(FALSE) { ## I removed the 'includeThreshold' option...there is now always a threshold value
      
    #if(input$includeThreshold=='No') {
      ## IF NO THRESHOLD IS PROVIDED, PROVIDE A TABLE OF THE LARGEST THREE ERRORS
      ## the Statement and the table will change, depending on whether the user wants the results
      ## reported as length errors or as % error...
      results_out$lengtherrors_summary = switch(input$report_as, 
             'Length error (mm)' = "No error specification was provided. Displaying the three largest length errors. Units are mm.",
             'Percentage of reference length' = "No error specification was provided. Displaying the three largest percent errors. Units are mm."
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
    
    ## CHECK FOR BAD TARGETS -- punt the 'R_pretty' dataset for the Historic dataset, if applicable
    results_out$Rpretty_test <- create_pretty_R(results_out$R2, data_name = dataname_test, target_names = data2_targetList)

    ## create the statements about the standard deviations in the angular/ranging residuals.
    results_out$testdata_sd = round(sqrt(diag(round(results_out$SigmaHat2,3))),2)
    results_out$testdata_SDstatements = HTML(
      paste("The standard deviation in the azimuth angle residuals is", results_out$testdata_sd[1], "arcsec.<br>", 
            "The standard deviation in the elevation angle residuals is", results_out$testdata_sd[2], "arcsec.<br>",
            "The standard deviation in the ranging residuals is", results_out$testdata_sd[3], "mm."))
    
    ##*********************
    ##**PART II -- CONDITIONAL ON WHETHER HISTORICAL COMPARISON IS BEING DONE**
    ##* Do the same Part II on the historical ('baseline') data
    if(input$assessment_type == "Verification Assessment") {
      incProgress(3/7, detail = "rigid body transformation on historic data")
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
      ## send this vector to the results, so it can be called in the report
      results_out$basedata_sd = basedata_sd
      
      ## Step 3:
      ## Perform the hypothesis test
      ## Step 3: test equality of the covariance matrices
      incProgress(4/7, detail = "Part II analysis")
      p2_results = TLS_cov_check(results_out$X1, results_out$X2, conf.level = 1-results_out$alpha)
      results_out$test_results = p2_results$results
      #results_out$p2_conclusion = p2_results$conclusion
      #results_out$p2_interpretation = p2_results$interpretation
      results_out$pvalue = p2_results$pvalue
      pval_clean = ifelse(p2_results$pvalue < 0.001, "<0.001", round(p2_results$pvalue,3))
      
      ## full statement on hypothesis test results
      results_out$p2_interpretation = paste0("The statistical methodology comparing the precision in the Verificaiton data to the precision in the Baseline data results in a p-value of ", pval_clean, ". ", p2_results$interpretation)  
                                        
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
      results_out$Rpretty_base <- create_pretty_R(results_out$R1, data_name = dataname_base, target_names = data1_targetList)
      
      
      ##*********************
      ##* MCS ---  PROPAGATE SIGMAHAT TO LENGTHS
      ##*            -- this is conditional on whether the hypothesis test was significant
      ##*            AND on whether any of the length errors are > MPE
      ##*********************
      
      if(nrow(results_out$lengtherrors_subtable)==0 & results_out$pvalue<results_out$alpha){
      incProgress(5/7, detail = "Monte Carlo simulation - this will take some time")
      # 1. Initialize the SECOND (inner) progress bar
      # We set max to nIt so the value represents the actual iteration count
      mc_progress <- shiny::Progress$new(session, min = 1, max = results_out$nIt_mcs)
      mc_progress$set(message = "Running Monte Carlo", value = 0)
      
      # Close the inner bar automatically when this block finishes or fails
      on.exit(mc_progress$close())
      
      # 2. Call your function, passing the progress object
      set.seed(results_out$seed_value)
      expected_errors <- obtain_expected_errors(Zc = Zc2_named, 
                                                ref_lengths = Dtape, SigmaHat = results_out$SigmaHat2, 
                                                nIt = results_out$nIt_mcs, 
                                                EE_scale = 3,
                                                progress_obj = mc_progress)  # Pass the object here
      incProgress(6/7, detail = "MCS finished")
      ## now add in the column names that the plotting function will expect
      results_out$expected_errors = expected_errors %>%
        mutate(lengthSD_MCS = round(lengthSD_MCS, 3), 
               EER = round(EER, 3), 
               EER_pct = round(EER/ReferenceLength*100,2))
      
      ## If a threshold was included, count how many expected errors are larger
      ## than the threshold, and report the values that were, if applicable
      ## 
      if(FALSE) { # includeThreshold value has been removed...there is always a threshold value now
        
      #if(input$includeThreshold=='No') {
        ## IF NO THRESHOLD IS PROVIDED, PROVIDE A TABLE OF THE LARGEST THREE ERRORS
        ## the Statement and the table will change, depending on whether the user wants the results
        ## reported as length errors or as % error...
        results_out$expectederrors_summary = switch(input$report_as, 
                                                    'Length error (mm)' = "No error specification was provided. Displaying the three largest maximum expected length errors. Units are mm.",
                                                    'Percentage of reference length' = "No error specification was provided. Displaying the three largest maximum expected errors as a percent of reference length. Units are mm."
        )
        results_out$expectederrors_subtable = switch(input$report_as,
                                                     'Length error (mm)' = (results_out$expected_errors %>% arrange(desc(abs(EER))))[1:3,], 
                                                     'Percentage of reference length' = (results_out$expected_errors %>% arrange(desc(abs(EER_pct))))[1:3,] 
        )
      } else{
        ## First make a generic version of the expectederror table so I can feed it into my 'statements' function
        gtab_MCS <- results_out$expected_errors %>% 
          reframe(ID, ReferenceLength, Position,
                  y = EER, y_pct = EER_pct)
        ## use the 'statements' function on the generic table
        expectederrors_exceed = lengthError_statement(table = gtab_MCS, t_val = results_out$threshold, 
                                                      report_as = results_out$error_reporting, section = "MCS")
        results_out$expectederrors_summary = expectederrors_exceed$statement
        results_out$expectederrors_subtable = expectederrors_exceed$table
      }
      } else { ## IF THE HYPOTHESIS TEST WAS NOT SIGNIFICANT, NEED TO MAKE SOME GENERAL STATEMENTS
        results_out$expectederrors_summary = NULL
        results_out$expectederrors_subtable = NULL
      }
      
    }  # <-- ENDS THE PART II COMPARISON
    #browser()
    
    return(results_out)
  })
#browser()
  })

  
  # ------------------------------------------------------------------
  # COMPLIANCE BOXES
  # ------------------------------------------------------------------
  compliance_results <- reactive({
    req(all_results())
    res_out <- all_results()
    
    # ----------------------------------------------------------------
    # STEP 1: EVALUATE PART I STATUS (ACCURACY)
    # ----------------------------------------------------------------
    # If the subtable contains 0 rows, no length errors exceeded the MPE 
    partI_status <- if (nrow(res_out$lengtherrors_subtable) == 0) "COMPLIANT" else "NON-COMPLIANT" 
    
    # ----------------------------------------------------------------
    # STEP 2: EVALUATE PART II HYPOTHESIS TEST STATUS (PRECISION)
    # ----------------------------------------------------------------
    partII_htest_status <- if (input$assessment_type == "Baseline Assessment") {
      "N/A" # No compliance designation applied to Part II during Baseline tracks 
    } else if (res_out$pvalue >= res_out$alpha) {
      "COMPLIANT" # No significant change in precision 
    } else {
      "NON-COMPLIANT" # Significant change in precision detected 
    }
    
    # ----------------------------------------------------------------
    # STEP 3: EVALUATE PART II EER STATUS (EXPECTED ERROR RANGE)
    # ----------------------------------------------------------------
    partII_eer_status <- if (input$assessment_type == "Baseline Assessment" || is.null(res_out$expected_errors)) {
      "N/A" # Simulation wasn't run or is not applicable 
    } else if (nrow(res_out$expectederrors_subtable) == 0) {
      "COMPLIANT" # All calculated EER values are less than or equal to the MPE 
    } else {
      "NON-COMPLIANT" # One or more EER values exceed the MPE 
    }
    
    # ----------------------------------------------------------------
    # STEP 4: COMBINE COMPONENT EVALUATIONS FOR THE OVERALL VERDICT
    # ----------------------------------------------------------------
    overall_meta <- list()
    
    # --- TRACK 1: BASELINE ASSESSMENT VERDICTS ---
    if (input$assessment_type == "Baseline Assessment") {
      if (partI_status == "COMPLIANT") {
        overall_meta <- list(
          label       = "COMPLIANT (Baseline Established)",
          class       = "success",
          description = "All 24 length errors are less than the MPE. Instrument accuracy is within specifications.",
          action      = "Establish the baseline dataset and maintain the regular IPA schedule."
        )
      } else {
        overall_meta <- list(
          label       = "NON-COMPLIANT",
          class       = "danger",
          description = "At least one of the 24 length errors exceeds the MPE. Instrument accuracy does not meet specifications.",
          action      = "Consider a larger MPE or contact the instrument manufacturer."
        )
      }
    }
    
    # --- TRACK 2: VERIFICATION ASSESSMENT VERDICTS (TABLE 2 MATRIX) ---
    if (input$assessment_type == "Verification Assessment") {
      
      # 1. Full Compliance [cite: 137, 169]
      if (partI_status == "COMPLIANT" && partII_htest_status == "COMPLIANT") {
        overall_meta <- list(
          label       = "COMPLIANT (Full)",
          class       = "success",
          description = "Length errors are within the MPE and the instrument's precision has not significantly changed since the baseline data were collected.",
          action      = "Maintain regular IPA schedule."
        )
      }
      
      # 2. Acceptable Precision Variance [cite: 140, 169]
      else if (partI_status == "COMPLIANT" && partII_htest_status == "NON-COMPLIANT" && partII_eer_status == "COMPLIANT") {
        overall_meta <- list(
          label       = "COMPLIANT (Acceptable Precision Variance)",
          class       = "success", 
          description = "Length errors are within the MPE. The instrument's precision has changed from the baseline data, but the current precision state is not expected to cause length errors to exceed the MPE.",
          action      = "Maintain regular IPA schedule."
        )
      }
      
      # 3. Potential Marginal Degradation [cite: 143, 169]
      else if (partI_status == "COMPLIANT" && partII_htest_status == "NON-COMPLIANT" && partII_eer_status == "NON-COMPLIANT") {
        overall_meta <- list(
          label       = "MARGINALLY COMPLIANT (Potential Early Degradation)",
          class       = "warning", 
          description = "Length errors are currently within the MPE. However, precision has changed significantly from the baseline data and the current precision state may cause length errors to exceed the MPE.",
          action      = "Consider performing IPA more frequently."
        )
      }
      
      # 4. Accuracy Exceedance with Stable Precision [cite: 146, 169]
      else if (partI_status == "NON-COMPLIANT" && partII_htest_status == "COMPLIANT") {
        overall_meta <- list(
          label       = "MARGINALLY COMPLIANT (Accuracy Exceedance with Stable Precision)",
          class       = "warning",
          description = "Length errors are not within the MPE. However, precision has not changed significantly from the baseline data. This may indicate that the MPE is too stringent.",
          action      = "Consider using instrument for use cases where a larger MPE is appropriate."
        )
      }
      
      # 5. Full Noncompliance [cite: 150, 169]
      else if (partI_status == "NON-COMPLIANT" && partII_htest_status == "NON-COMPLIANT") {
        overall_meta <- list(
          label       = "NON-COMPLIANT",
          class       = "danger",
          description = "Length errors are not within the MPE and instrument precision has changed significantly from the baseline data.",
          action      = "Consider withdrawing instrument from service for professional maintenance."
        )
      }
    }
    
    # Return everything as a clean structural package to feed into the UI components
    return(list(
      partI      = partI_status,
      htest      = partII_htest_status,
      eer        = partII_eer_status,
      label      = overall_meta$label,
      class      = overall_meta$class,
      description= overall_meta$description,
      action     = overall_meta$action
    ))
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
  
  # PART I: COMPLIANCE STATEMENT BOX
  output$partI_summary_box <- renderUI({
    req(compliance_results())
    res <- compliance_results()
    
    box_class <- if(res$partI == "COMPLIANT") "success" else "danger"
    icon_name <- if(res$partI == "COMPLIANT") "check-circle" else "times-circle"
    sub_text  <- if(res$partI == "COMPLIANT") {
      "All 24 calculated length errors fall within the Maximum Permissible Error"
    } else {
      "One or more of the 24 calculated length errors exceed the Maximum Permissible Error"
    }
    
    div(class = paste0("alert alert-", box_class), role = "alert",
        style = "margin-top: 15px; padding: 15px; font-size: 1.15em;",
        tags$strong(icon(icon_name), " Part I Evaluation: ", res$partI),
        tags$p(sub_text, style = "margin: 5px 0 0 24px; font-size: 0.95em;")
    )
  })
  
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
  ## renderPrint changed to renderDT
  output$actual_table_content <- renderPrint({
    # This sends the actual data to the placeholder created above
    if(nrow(all_results()$lengtherrors_subtable) > 0) {
      all_results()$lengtherrors_subtable
    } else {
      return(NULL)
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
      plot_as = all_results()$error_reporting, 
      my_title = all_results()$dataname_test
    )
  })
  
  # 2. Render the plot in the web app (later we will render it in the downloadable report format)
  output$errorPlot <- renderPlot({
    error_plot_reactive()
  }, res = 96)
  
  #############################
  ## HISTORICAL COMPARISON 
  ##
  ## PART I - HISTORICAL
  ##
  ## Statement ('x out of k length errors exceed the threshold')
  output$length_summary_hist <- renderUI({
    if(is.null(all_results()$lengtherrors_summary_hist)) {
      return(NULL)}
    HTML(all_results()$lengtherrors_summary_hist)
  })
  
  ## Creating a 'conditional' table for the historical Part I results (no table reported if threshold given and nothing exceeds threshold)
  # 1. The Dynamic UI Container
  output$lengtherrors_subtable_hist <- renderUI({
    if(nrow(all_results()$lengtherrors_subtable_hist)==0) {
      return(NULL)
    } else {
      # Create the placeholder with a unique ID string
      verbatimTextOutput("actual_table_content_hist")
    }
  })
  # 2. The Actual Content Provider
  output$actual_table_content_hist <- renderPrint({
    # This sends the actual data to the placeholder created above
    all_results()$lengtherrors_subtable_hist
  })
  
  ## 
  ## And now plot the historic Part I plot
  ##
  # 1. Create the plot as a reactive object
  error_plot_reactive_hist <- reactive({
    # Check if data exists
    if(is.null(all_results()$A_F_LengthError_table_hist)) {
      return(NULL)
    }
    # Call your custom plotting function
    partIerrorplot(
      dat = all_results()$A_F_LengthError_table_hist, 
      threshold = all_results()$threshold, 
      plot_as = all_results()$error_reporting, 
      my_title = all_results()$dataname_base, 
      color = "no"
    )
  })
  
  # 2. Render the plot in the web app (later we will render it in the downloadable report format)
  output$errorPlot_hist <- renderPlot({
    error_plot_reactive_hist()
  }, res = 96)
  
  #############################
  ##** PART II ANALYSIS
  ##*
  
  # PART II COMPLIANCE BOX
  output$partII_summary_box <- renderUI({
    req(compliance_results())
    res <- compliance_results()
    
    if (input$assessment_type == "Baseline Assessment") {
      div(class = "alert alert-info", role = "alert",
          style = "margin-top: 15px; padding: 15px; font-size: 1.15em;",
          tags$strong(icon("info-circle"), " Part II Evaluation: Baseline Assessment"),
          tags$p("Part II has no compliance evaluation during a Baseline Assessment.", style = "margin: 5px 0 0 24px; font-size: 0.95em;")
      )
    } else {
      h_style <- if(res$htest == "COMPLIANT") "text-success" else "text-danger"
      
      eer_display_text <- if(res$partI == "NON-COMPLIANT" && res$htest == "NON-COMPLIANT") {
        "N/A (Simulation not required)"
      } else if (res$eer == "N/A") {
        "N/A (Simulation not required)"
      } else {
        res$eer
      }
      
      eer_style <- if(res$eer == "COMPLIANT") "text-success" else if(res$eer == "NON-COMPLIANT") "text-danger" else "text-muted"
      
      div(class = paste0("alert alert-", res$class), role = "alert",
          style = "margin-top: 15px; padding: 15px; font-size: 1.15em;",
          tags$strong(icon(if(res$class == "success") "check-circle" else if(res$class == "warning") "exclamation-triangle" else "times-circle"), " Part II Evaluation:"),
          tags$ul(style = "margin: 5px 0 0 10px; font-size: 0.95em;",
                  tags$li(tags$strong("Hypothesis Test Result: "), tags$span(class = h_style, res$htest)),
                  tags$li(tags$strong("Expected Error Range (EER) Result: "), tags$span(class = eer_style, eer_display_text))
          )
      )
    }
  })
  
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
  #output$p2_conclusion <- renderText({
  #  if(is.null(all_results()$p2_conclusion)) {
  #    return(NULL)
  #  }
  #  return(all_results()$p2_conclusion)
  #})
  
  output$p2_interpretation <- renderUI({
    if(is.null(all_results()$p2_interpretation)) {
      return(NULL)
    }
    tags$div(
      style = "max-width: 50%; line-height: 1.5; text-align: justify;",
      HTML(all_results()$p2_interpretation)
    )
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
  #output$ellipsePlot <- renderPlot({
  #  if(is.null(all_results()$expected_errors)) {
  #    return(NULL)
  #  }
  #  my_ellipse_plot(all_results()$Xcomb)
  #}, res = 96)
  
  
  # This waits for all_results() to exist, then decides to show the link or the plot
  output$ellipse_container <- renderUI({
    req(all_results()) # Don't show anything until analysis has run at least once
    if(is.null(all_results()$Xcomb)) {
          return(NULL)
      } else{
        if (show_ellipse() == FALSE) {
          ## Show the 'Open plot link
          actionLink("view_plot_link", "Show data ellipse plot to compare precisions")
        } else {
          tagList(
            div(style = "margin-bottom: 10px;",
                actionLink("hide_plot_link", "Hide data ellipse plot", 
                           icon = icon("times"), 
                           style = "color: #d9534f; font-size: 0.9em;")
            ),
            plotOutput("ellipsePlot", width = "600px", height = "600px")
          )
        }
      }
  })
  
  # 6. THE PLOT RENDERER
  output$ellipsePlot <- renderPlot({
    req(show_ellipse(), all_results()$Xcomb)
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
  output$MCS_results_container <- renderUI({
    # 1. Centralized check: If no MCS data, show nothing
    req(all_results()$expected_errors)
    tagList(
      h2("Effect of the Current Precision on Length Errors"),
      # Inlined EER Explanation
      tags$p("Since there is a statistically significance change in the precision, the effect of the instrument’s 
           current spherical precision on the length measurements has been 
           estimated using a Monte Carlo simulation. The “expected error range” (EER) value computed from 
           this simulation is the range of error, for a given length and position, that is expected in the 
           instrument’s measurement due to the current spherical precision.",
             style = "max-width: 50%; line-height: 1.5; text-align: justify;"),
      br(),
      
      # Inlined Summary Statement
      # We pull the value directly from all_results()
      all_results()$expectederrors_summary,
      
      # Conditional Table Placeholder
      # We only show the verbatim box if there are rows to show
      if (nrow(all_results()$expectederrors_subtable) > 0) {
        verbatimTextOutput("actual_table_content_EE")
      },
      br(),
      # Plot Placeholder
      plotOutput("expectederrorPlot", width = "700px", height = "350px")
    )
  })
  
  ## Explain what the EER values are (this is always shown, if the MCS simulation was run)
  #output$EER_explanation <- renderUI({
    # Logic check
  #  if ( !is.null(all_results()$expected_errors)) {
      # Use tags$p, tags$span, or just HTML text
  #    return(tags$p("Since there is a statistically significance change in the precision, he effect of the instrument’s 
  #                  current spherical precision on the length measurements has been 
  #                  estimated using a Monte Carlo simulation. The “expected error range” (EER) value computed from 
  #                  this simulation is the range of error, for a given length and position, that is expected in the 
  #                  instrument’s measurement due to the current spherical precision.",
  #                  style = "max-width: 50%; line-height: 1.5; text-align: justify;"))
  #  } else {
      # Returning NULL ensures the UI element is completely empty/removed
  #    return(NULL)
  #  }
  #})

  ## Overall statement "x out of y length errors are greater than *threshold*"
 # output$expectederrors_summary <- renderUI({
#    if(is.null(all_results()$expectederrors_summary)) {
#      return(NULL)}
#    return(all_results()$expectederrors_summary)
#  })

  ## Creating a 'conditional' table for the Expected Errors
  # 1. The Dynamic UI Container
#  output$expectederrors_subtable <- renderUI({
#    # Check if the table has data
#    if ( !is.null(all_results()$expected_errors) && nrow(all_results()$expectederrors_subtable) > 0) {
      # Create the placeholder with a unique ID string
#      verbatimTextOutput("actual_table_content_EE")
#    } else {
#      return(NULL)
#    }
#  })
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

      # Render the plot only if condition is met
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
  ## Metadata Summaries
  ##
  # OVERALL ASSESSMENT BOX
  output$overall_assessment_summary <- renderUI({
    req(compliance_results())
    res <- compliance_results()
    
    border_color <- if(res$class == "success") "#3c763d;" else if(res$class == "warning") "#8a6d3b;" else "#a94442;"
    
    wellPanel(
      style = paste0("border-left: 6px solid ", border_color, " background-color: #fcfcfc; padding: 20px; margin-top: 10px;"),
      h3(paste("Assessment Conclusion:", res$label), 
         style = paste0("margin-top: 0; font-weight: bold; color: ", border_color)),
      p(strong("Finding: "), res$description, style = "font-size: 1.1em;"),
      p(strong("Recommendation: "), res$action, style = "font-size: 1.1em; color: #333;")
    )
  })
  
  output$summary_currentdata <- renderUI({
    req(all_results()$filename_TLStest)
    # Use HTML to recognize line breaks
    HTML(paste0(
      "<h4>TLS data</h4>",
      "<div style='margin-left: 20px;'>", # Start indentation
      "<b>filename:</b> ", all_results()$filename_TLStest, "<br/>",
      "<b>nickname:</b> ", all_results()$dataname_test, "<br/>",
      "<b># of targets:</b> ", all_results()$nT_test, "<br/>",
      "<b># of positions:</b> ", all_results()$nP_test, "<br/>", 
      "<b>units:</b> ", all_results()$TLS_UnitStatement, "<br/>",
      "</div>", # End indentation
      "<h4>Reference Lengths</h4>", 
      "<div style='margin-left: 20px;'>", # Start indentation
      "<b>filename:</b> ", all_results()$filename_reflengths, "<br/>",
      "<b>units:</b> ", all_results()$tapedata_summary,
      "</div>" # End indentation
    ))
  })
  
  output$summary_historicdata <- renderUI({
    req(all_results()$filename_TLShist)
    # Use HTML to recognize line breaks
    HTML(paste0(
      "<h4>TLS data</h4>",
      "<div style='margin-left: 20px;'>", # Start indentation
      "<b>filename:</b> ", all_results()$filename_TLShist, "<br/>",
      "<b>nickname:</b> ", all_results()$dataname_base, "<br/>",
      "<b># of targets:</b> ", all_results()$nT_base, "<br/>",
      "<b># of positions:</b> ", all_results()$nP_base, "<br/>", 
      "<b>units:</b> ", all_results()$historicTLS_UnitStatement, "<br/>",
      "</div>", # End indentation
      "<h4>Reference Lengths</h4>", 
      "<div style='margin-left: 20px;'>", # Start indentation
      "<b>filename:</b> ", all_results()$filename_reflengths_hist, "<br/>",
      "<b>units:</b> ", all_results()$reflengths_hist_summary,
      "</div>" # End indentation
    ))
  })
  
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
    
    ##################################################
    ## RESIDUAL PLOTS -- CURRENT DATA
    # --- Render the Plot UI Once ---
    ## 1. THE UI CONTAINER (This handles the toggling)
    output$resPlot_container_test <- renderUI({
      req(all_results()$Rpretty_test)
      tagList(
        conditionalPanel(
          condition = "output.show_rp_current_state == false",
          actionLink("view_rp_current", "Residual plot")
        ),
        
        conditionalPanel(
          condition = "output.show_rp_current_state == true",
          div(style = "margin-bottom: 10px;",
              actionLink("hide_rp_current", "Hide residual plot", 
                         icon = icon("times"), 
                         style = "color: #d9534f; font-size: 0.9em;")
          ),
          HTML(paste("Angular residuals are reported in arcseconds. Ranging residuals are reported in mm.")),
          plotOutput("resPlot_test", width = "600px", height = "600px")
        )
      )
    })
    
    ## 2. THE BRIDGE (Tells JavaScript the state of your R reactiveVal)
    output$show_rp_current_state <- reactive({ show_rp_current() })
    outputOptions(output, "show_rp_current_state", suspendWhenHidden = FALSE)
    
    ## CREATE PLOT AS REACTIVE OBJECT
    # Create the plot as a reactive object
    res_plot_reactive_test <- reactive({
      # Use req to ensure data exists
      req(all_results()$Rpretty_test)
      # Call the plotting function
      plot_my_residuals(all_results()$Rpretty_test)
    })
    
    ## 3. THE PLOT RENDERER 
    output$resPlot_test <- renderPlot({
      # Optional: only calculate the plot if the user actually wants to see it
      # This saves processing power
      req(show_rp_current())
      ## call the reactive plot
      res_plot_reactive_test()
      #plot_my_residuals(all_results()$Rpretty_test)
    }, res = 96)
    
    ##################################################
    ## RESIDUAL PLOTS -- HISTORIC DATA
    # --- Render the Plot UI Once ---
    ## 1. THE UI CONTAINER (This handles the toggling)
    output$resPlot_container_base <- renderUI({
      req(all_results()$Rpretty_base)
      tagList(
        conditionalPanel(
          condition = "output.show_rp_historic_state == false",
          actionLink("view_rp_historic", "Residual plot (baseline data)")
        ),
        
        conditionalPanel(
          condition = "output.show_rp_historic_state == true",
          div(style = "margin-bottom: 10px;",
              actionLink("hide_rp_historic", "Hide residual plot", 
                         icon = icon("times"), 
                         style = "color: #d9534f; font-size: 0.9em;")
          ),
          HTML(paste("Angular residuals are reported in arcseconds. Ranging residuals are reported in mm.")),
          plotOutput("resPlot_base", width = "600px", height = "600px")
        )
      )
    })
    ## 2. THE BRIDGE (Tells JavaScript the state of your R reactiveVal)
    output$show_rp_historic_state <- reactive({ show_rp_historic() })
    outputOptions(output, "show_rp_historic_state", suspendWhenHidden = FALSE)
    
    ## CREATE PLOT AS REACTIVE OBJECT
    # Create the plot as a reactive object
    res_plot_reactive_base <- reactive({
      # Check if data exists
      if(is.null(all_results()$Rpretty_base)) {
        return(NULL)
      }
      # Call the plotting function
      plot_my_residuals(all_results()$Rpretty_base)
    })
    
    ## 3. THE PLOT RENDERER 
    output$resPlot_base <- renderPlot({
      # Optional: only calculate the plot if the user actually wants to see it
      # This saves processing power
      req(show_rp_historic())
      # Call the reactive expression
      res_plot_reactive_base()
      #plot_my_residuals(all_results()$Rpretty_base)
    }, res = 96)
    
    
#    output$resPlot_base <- renderPlot({
#      if(is.null(all_results()$Rpretty_base)) {
#        return(NULL)
#      }
#      plot_my_residuals(all_results()$Rpretty_base)
 #   }, res = 96)
    
#    output$resPlot_test <- renderPlot({
#      if(is.null(all_results()$Rpretty_test)) {
#        return(NULL)
#      }
#      plot_my_residuals(all_results()$Rpretty_test)
#    })
    
    output$residualStatement <- renderUI({
      req(all_results())
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
        # 1. Start the progress bar
        withProgress(message = 'Generating Report', 
                     detail = 'This may take a few moments...', 
                     value = 0, {
                       
          # 2. Increment the bar to show it started
          incProgress(0.3, detail = "Gathering data and plots...")
        
        # Copy the report file to a temporary directory
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # --- NEW PLOT GENERATION STEP ---
        # 1. Create a temporary file path for the ellipse plot
        ellipse_plot_path <- file.path(tempdir(), "temp_ellipse_plot.png")
        
        # 2. If the data exists, generate and save the plot using your function
        if (!is.null(all_results()$Xcomb)) {
          p_ellipse <- my_ellipse_plot(all_results()$Xcomb)
          
          # Use ggsave to "bake" the Greek letters into a high-res image
          # 300 DPI is standard for professional print quality
          ggplot2::ggsave(ellipse_plot_path, plot = p_ellipse, 
                          width = 8, height = 8, units = "in", dpi = 300)
        } else {
          ellipse_plot_path <- NULL
        }
        
        # Set up parameters to pass to Rmd document
        # Set up parameters to pass to Rmd document
        params <- list(
          report_title  = "TLS Interim Performance Assessment Results",
          
          # ================================================================
          # ASSESSMENT TYPE & COMPLIANCE VERDICTS
          # ================================================================
          assessment_type   = input$assessment_type,
          overall_label     = compliance_results()$label,
          overall_desc      = compliance_results()$description,
          overall_action    = compliance_results()$action,
          partI_status      = compliance_results()$partI,
          partII_htest      = compliance_results()$htest,
          partII_eer        = compliance_results()$eer,
          
          # ================================================================
          # DATA UNDER TEST META STUFF
          # (Baseline Data if Baseline Track; Verification Data if Verification Track)
          # ================================================================
          has_baseline_comp   = all_results()$partII_htest,                     ## "Yes" or "No", indicating whether historic comp was performed
          filename_TLS_test   = all_results()$filename_TLStest,   
          dataname_test       = all_results()$dataname_test, 
          nT_test             = all_results()$nT_test,                         
          nP_test             = all_results()$nP_test,                         
          TLS_UnitStatement   = all_results()$TLS_UnitStatement,     
          filename_reflengths = all_results()$filename_reflengths, 
          tapedata_summary    = all_results()$tapedata_summary,       
          
          # ================================================================
          # RENAMED REQUIREMENT 3: BASELINE COMPARISON DATA META STUFF
          # (Only populated and utilized during a Verification Track)
          # ================================================================
          filename_TLS_baseline       = all_results()$filename_TLShist,       
          dataname_baseline           = all_results()$dataname_base,             
          nT_baseline                 = all_results()$nT_base,                         
          nP_baseline                 = all_results()$nP_base,                         
          baseline_TLS_UnitStatement  = all_results()$historicTLS_UnitStatement, 
          filename_reflengths_baseline = all_results()$filename_reflengths_hist, 
          reflengths_baseline_summary = all_results()$reflengths_hist_summary,   
          
          # ================================================================
          # PART I RESULTS (ACCURACY)
          # ================================================================
          # Data Under Test Accuracy
          length_summary         = all_results()$lengtherrors_summary,
          error_plot             = error_plot_reactive(),                  
          error_table            = all_results()$A_F_LengthError_table,       
          error_table_sub        = all_results()$lengtherrors_subtable,   
          lengtherror_statements  = all_results()$A_F_LengthError_statements, 
          mpe_value               = all_results()$threshold,
          unc_ref                 = all_results()$unc_ref,
          
          # Baseline Comparison Target Accuracy (Verification track only)
          length_summary_baseline     = all_results()$lengtherrors_statement_hist_clean, 
          error_table_baseline        = all_results()$A_F_LengthError_table_hist,     
          error_table_sub_baseline    = all_results()$lengtherrors_subtable_hist, 
          error_plot_baseline         = error_plot_reactive_hist(),              
          lengtherror_statements_baseline = all_results()$A_F_lengthError_statements_hist, 
          
          # ================================================================
          # PART II RESULTS (PRECISION)
          # ================================================================
          sd_test           = all_results()$testdata_SDstatements,           
          sigmaHat_test     = all_results()$SigmaHat2,                 
          sigmaHat_baseline = all_results()$SigmaHat1,                 
          baseline_data_sd  = all_results()$basedata_sd,                 
          
          # Precision Comparison & Simulations
          p2_interpretation = all_results()$p2_interpretation,                  ## interpretation of the hypothesis test
          Htest_details     = all_results()$test_results,                       ## details from the hypothesis test
          ellipse_plot_path = ellipse_plot_path,                                ## pass the path to the saved ellipse plot
          expectederrors_summary = all_results()$expectederrors_summary,        ## statement about how many EER values exceed threshold
          expectederror_plot = expectederror_plot_reactive(),
          seed_value        = all_results()$seed_value,
          nIt_mcs           = all_results()$nIt_mcs,
          expected_errors   = all_results()$expected_errors,
          alpha             = all_results()$alpha,
          
          # ================================================================
          # VISUAL RESIDUALS & SUPPLEMENTAL NOTES
          # ================================================================
          notes             = input$report_notes,                               ## User-given notes
          resPlot_current   = res_plot_reactive_test(),                         ## Residual plot for Data Under Test (Baseline or Verification, depending on track)
          resPlot_baseline  = res_plot_reactive_base()                          ## Residual plot for Baseline data (only if performing Verification Assessment)
        )
       
        
        # 3. Increment again before the "heavy lifting"
        incProgress(0.3, detail = "Rendering PDF (This may take a few moments)...")
        
        # Knit the document
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        # 4. Final step
        incProgress(0.4, detail = "Done!")
        })
      }
    )
    
      
  ###############################
  ##**Information
  ##*
  #output$info <- renderUI({
    # We wrap the entire text string in a div with 60% width
   # div(style = "width: 60%; line-height: 1.6;",
    #  HTML(paste("This web app implements the required calculations 
    #  to perform the OSAC CSIR `Terrestrial Laser Scanner Performance Assessment Test Procedure'. 
    #  This test procedure consists of two parts:<br><br>", 
    #  "Part I: Test of the instrument's target-to-target accuracy;<br>", 
    #  "Part II: Statement of the instrument's spherical precision.<br>", 
    #  "<br>",
    #  "If a historic comparison is made, a <a href='https://doi.org/10.1111/1556-4029.70256' target='_blank'>statistical procedure</a> 
    #  is applied to test if the instrument's spherical precision has significantly changed. If there is a statistically significant
    #  difference in the spherical precision, the effect of the current instrument precision on the length measurements is estimated using a Monte Carlo simulation.<br><br>",
    
  #    "The user manual for this webapp can be downloaded [here]. Additional information can be found in the OSAC standard, 
  #    [this paper], and [this other paper]."
  #    ))
  #  )
  #})
    
    output$info <- renderUI({
      # Main layout container opens
      div(style = "max-width: 750px; line-height: 1.6; color: #333333; padding-right: 15px;",
          
          # Main Header
          h2("Terrestrial Laser Scanner Interim Performance Assessment WebApp", 
             style = "margin-top: 0; color: #2c3e50; font-weight: bold;"),
          
          p("This web application is the official tool for executing the calculations and compliance analyses required by the ",
            em("Standard for Terrestrial LiDAR Scanner (TLS) Calibration and Performance Assessment."),
            " This standard is currently under development by the OSAC CSIR Subcommittee."
          ),
          
          p("This application is designed strictly for data analysis. It does not provide instruction on the execution of the IPA procedure, nor does it perform any cleaning or formatting of data files."),
          
          # Gateway Qualifier Section
          h3("Am I in the right place?", style = "color: #2c3e50; margin-top: 25px; font-weight: 600;"),
          
          p(strong("You are ready to use this application if:")),
          tags$ul(
            tags$li(strong("You have completed the IPA testing procedure:"), " Your data were collected in accordance with the multi-position setup detailed in the draft standard."),
            tags$li(strong("Your data are pre-processed and correctly formatted:"), " You have already extracted and matched target coordinates from your individual scans into the required single-file format. Your reference lengths have been recorded appropriately, and all files conform to the layout specifications required by this application."),
            tags$li(strong("You understand what analyses are performed by this application:"), " You have determined whether your data represent a Baseline Assessment or a Verification Assessment, and you understand the evaluation criteria that will be applied to your datasets.")
          ),
          
          p(strong("You are not ready to use this application if:")),
          tags$ul(
            tags$li("You have not yet performed the IPA testing procedure."),
            tags$li("You are unsure what reference lengths or target files are required."),
            tags$li("Your data files are still in a raw scanner format.")
          ),
          
          p(em("If you need additional information, please consult the draft standard document and the User Manual for this web application before attempting to upload your files.")),
          
          # Structural Scope Section
          h3("What This Application Evaluates", style = "color: #2c3e50; margin-top: 25px; font-weight: 600;"),
          
          p("Once your data are uploaded, this application automatically assesses your instrument’s compliance across the following two parts:"),
          tags$ul(
            tags$li(strong("Part I – Target-to-Target Accuracy:"), " Evaluating how closely the scanner's measurements match calibrated reference lengths."),
            tags$li(strong("Part II – Instrument Precision:"), " Analyzing the consistency of the scanner's internal measurements across multiple positions to detect significant shifts in performance.")
          ),
          
          # Section 2: Direct Signposts to Resources
          h3("Documentation & Resources", style = "color: #2c3e50; margin-top: 25px; font-weight: 600;"),
          p("Access the core project platform or download the user manual documentation directly below:"),
          
          # Flex container to display buttons neatly side-by-side
          div(style = "margin-top: 15px; display: flex; gap: 20px;",
              
              # Button A: External Portal (Muted Slate Outline)
              tags$a(href = "https://pages.nist.gov/tls-web-portal/", 
                     target = "_blank", 
                     class = "btn", 
                     style = "background-color: #f2f2f2; border: 1px solid #dcdcdc; color: #2c3e50; font-weight: 500; padding: 8px 16px;",
                     icon("external-link-alt"), " Open TLS Resource Portal"
              ),
              
              # Button B: Locally Hosted User Manual (Muted Slate Outline)
              tags$a(href = "manual.pdf", 
                     target = "_blank", 
                     class = "btn", 
                     style = "background-color: #f2f2f2; border: 1px solid #dcdcdc; color: #2c3e50; font-weight: 500; padding: 8px 16px;",
                     icon("file-pdf"), " Download User Manual"
              )
          ),
          
          br(),
          #p(tags$small(style = "color: #888888;", "Application Version: 1.1.0 | Core Engine Updated: 06/30/2026"))
      ) # <--- This parenthesis now correctly closes the main outer layout div
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
