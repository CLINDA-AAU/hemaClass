library(shiny)
library(shinysky)
library(RLumShiny)

#title <- "hemaClass" # Old title :) 
title <- HTML(paste0('<span class="rand"', 
                     'style="color: #FF6060; text-decoration: none">', 
                     icon("tint", lib = "glyphicon"), ' hemaClass</span>'))

# Conditions using in classification panel
abc.cond <- "input.getClassifications.indexOf('ABCGCB') != -1"
rtx.cond <- "input.getClassifications.indexOf('Rituximab (R)') != -1"
cyc.cond <- "input.getClassifications.indexOf('Cyclophosphamide (C)') != -1"
dox.cond <- "input.getClassifications.indexOf('Doxorubicin (H)') != -1"
vin.cond <- "input.getClassifications.indexOf('Vincristine (O)') != -1"
dex.cond <- "input.getClassifications.indexOf('Dexamethasone') != -1"
cho.cond <- "input.getClassifications.indexOf('Combined (CHO)') != -1"

shinyUI(
  navbarPage(
    title = title,
    theme = "bootstrap.css",
    tabPanel(
      'Home',   
      sidebarLayout(
        sidebarPanel(
          navlistPanel(
            "Information:",
            tabPanel("Welcome"),
            tabPanel("News"),
            tabPanel("Help"),
            tabPanel("Publications"),
            tabPanel("About"),
            well = FALSE,
            id = "nlp",
            widths = c(12,1)),
          width = 4),
        mainPanel(
          htmlOutput("mpContent")
        )
      )
    ), # End tabPanel "Home"
    
    tabPanel(
      'Load data',
      navbarPage(
        'Type of data',
        tabPanel(
          'CEL files',
          sidebarLayout(
            sidebarPanel(
              h3("Upload .CEL files"),
              busyIndicator("Pre-processing .CEL files", wait = 1000),
              fileInput("usrFiles",
                        "Please choose your .CEL files to classify:",
                        accept = "", multiple = TRUE),
              tags$hr(),
              conditionalPanel(
                condition = "output.showNormMethods == 0",
                h4("Please click here to download 4 .CEL files available for 
                   test use"),
                tags$a(href = 'http://hemaclass.org/celfiles.zip', 
                       class = "btn btn-default shiny-download-link", 
                       target = "_blank", icon("download"),
                       'Download test data.')
              ),
              
              conditionalPanel(
                condition = "output.showNormMethods!=0",
                h3("RMA Pre-processing"),
                selectInput(
                  "ChooseMethod", "Choose method",
                  list("Please select" = "blah",
                       "Use build-in reference" = "standardReference",
                       "Build a new reference" = "build",
                       "Upload a reference" = "upload",
                       "Cohort based RMA" = "RMA"
                  )
                ),
                conditionalPanel(
                  condition = "input.ChooseMethod == 'build'",
                  
                  fileInput("refFiles", 
                            label = "To build a laboratory specific 
                              reference, please upload .CEL files:",
                            accept = "", multiple = TRUE),
                  conditionalPanel(
                    condition = "output.showBuildRefButton!=0",
                    actionButton("buildreferenceButton",
                                 HTML(paste(icon("cog", lib = "glyphicon"), 
                                            "Build reference"))),
                    conditionalPanel(
                      condition = "output.showDownloadRefButton!=0",
                      tags$hr(),
                      helpText("Download reference to save time on next run:"),
                      downloadButton('downloadReference', 
                                     'Download reference for later use.')
                    )
                  )
                ),
                tags$hr()
              ),
              
              conditionalPanel(
                condition = "input.ChooseMethod == 'upload'",
                fileInput("refUpload", 
                          "Upload the file containing the reference:", 
                          accept = "", multiple = FALSE),
                tags$hr()
              ),
              
              conditionalPanel(
                condition = "input.ChooseMethod == 'standardReference'",
                selectInput(
                  "ChooseReference", "Choose a reference",
                  list("LLMPP CHOP"   = "LLMPPCHOP",  
                       "LLMPP R-CHOP" = "LLMPPRCHOP", 
                       "IDRC"         = "IDRC", 
                       "MDFCI"        = "MDFCI",
                       "CHEPRETRO"    = "CHEPRETRO",
                       "UAMS"         = "UAMS"
                  )),
                tags$hr() 
              ),
              conditionalPanel(
                condition = "output.showNormButton!=0",
                helpText("Normalize the files according to the chosen 
                         reference:"),
                actionButton("normalizeButton", 
                             HTML(paste(icon("cog", lib = "glyphicon"), 
                                        "Normalize files"))),
				tags$br(),
				tags$hr(),
								
				sliderInput("rle.iqr.threshold",
                            "Keep samples with RLE IQR less than:",
                            step = 0.01,
                            min = 0, max = 1.5, value = 0.6)
              )
              
              
            ), 
            mainPanel(
              shinyalert("shinyalertUploadCel", 
                         click.hide = FALSE),
              shinyalert("shinyalertSelectReference", 
                         click.hide = FALSE),
              shinyalert("shinyalertUploadCelSucces", 
                         auto.close.after = 10),
              shinyalert("shinyalertSelectReferenceSucess", 
                         auto.close.after = 10),
              shinyalert("shinyalertNormalizationSuccess", 
                         click.hide = FALSE, auto.close.after = 10),
              dataTableOutput("rle.stats"),
              conditionalPanel(
                condition = "output.showErrorprints!=0",
                verbatimTextOutput("start"), 
                verbatimTextOutput("start2"), 
                verbatimTextOutput("showBuildRefButton"),
                verbatimTextOutput("showNormButton"),
                verbatimTextOutput("showDownloadRefButton"),                         
                verbatimTextOutput("showNormMethods"),
                verbatimTextOutput("showErrorprints"))
            )
          )
        ), # End tabPanel "CEL files" 
        
        navbarMenu( 
          'Metadata', 
          tabPanel(
            'Input manually', 
            sidebarLayout( 
              sidebarPanel(
                helpText("You may also calculate IPI from clinical features."),
                checkboxInput(inputId = "IPIcalc",
                              label = "Calculate IPI", value = FALSE),
                
                helpText("Additional columns"),
                
                select2Input("Additionalcolumns", 
                             "Type in the names.",
                             choices = c("Age","LDH"),
                             selected = "IPI"),
                
                conditionalPanel(
                  condition = "input.IPIcalc",
                  helpText("You may change the settings for the IPI calculation."),
                  numericInput(inputId = "AGE.cut", label = "Get point when age > x", 60),
                  numericInput(inputId = "ECOG.cut", label = "Get point when ECOG > x", 1),
                  numericInput(inputId = "LDH.cut", label = "Get point when LDH > x", 1),
                  numericInput(inputId = "N.Extra.Nodal.cut", 
                               label = "Get point when number of extra nodal sites > x", 1),
                  numericInput(inputId = "Stage.cut", label = "Get point when Stage > x", 2)
                  
                ),
                tags$hr(), 
                downloadButton('downloadMetadataManual', 'Download metadata')
              ), 
              mainPanel(
                
                shinyalert("shinyalertInputMeta", click.hide = FALSE),
                
                conditionalPanel(
                  condition = "input.IPIcalc",
                  helpText("Input the clinical features for each patient below.",
                           "The IPI values are calculated automatically.")
                ),
                conditionalPanel(
                  condition = "!input.IPIcalc",
                  helpText("Input the IPI scores for each patient below.")
                ),
                div(class = "well container-fluid",
                    hotable("hotableMetadataManual"))
              )
            ) 
          ), # End tabPanel "Input manually"
          tabPanel(
            'Upload metadata', 
            sidebarPanel(
              h4("Upload file containing metadata"),
              busyIndicator("Processing the metadata", wait = 1000),
              fileInput("usrMeta", 
                        "Please upload the file storing the metadata", 
                        accept = "", multiple = FALSE),
              
              conditionalPanel(
                condition = "output.showReadMetaMethods=='txt'",
                
                helpText("Does the file contain a header"),
                checkboxInput("ExttxtHeader", label = "header", 
                              value = TRUE),
                selectInput(
                  "ExttxtSep", "Choose separater",
                  list("Tabulate" = "\t", "Semicolon" = ";",  
                       "comma" = ",", "Other" = "Other")),
                conditionalPanel(
                  condition = "input.ExttxtSep=='Other'", 
                  textInput(inputId = "ExttxtSepOther", 
                            "Type in the separator", value = ";"))
              ),
              
              conditionalPanel(
                condition = "output.showReadMetaMethods=='xls'|| output.showReadMetaMethods=='xlsx'",
                textInput(inputId = "ExtXLSsheet", 
                          "The sheet you want to read", 1)
                
              ),
              
              uiOutput("MetaUploadCelFileNames")
            ), # End sidebarPanel "Upload file containing metadata"
            
            mainPanel(
              shinyalert("shinyalertUploadMeta", click.hide = FALSE),
              shinyalert("shinyalertUploadMetaData", click.hide = FALSE),
              helpText("The Output below shows how the data is read."),
              dataTableOutput("uploadMetaData"),
              conditionalPanel(
                condition = "output.showReadMetaPrint!=0",
                verbatimTextOutput("showReadMetaPrint"), 
                verbatimTextOutput("showReadMetaMethods") 
                
              )
            )                          
          ) # End tabPanel "Upload metadata"
        ), # End tabPanel "Metadata"
        
        tabPanel( 
          'Build-in test data', 
          sidebarLayout(        
            sidebarPanel(
              busyIndicator("Loading data", wait = 1000),
              helpText("Select the dataset you want to analyse"),
              uiOutput("buildindataselector"),
              
              radioButtons("buildinShowData", "Show", 
                           choices = list("Metadata" = "Metadata",
                                          "Gene Expression" = "GEP"),
                           selected = "GEP")
            ),
            
            mainPanel(
              conditionalPanel(
                condition = "input.buildinShowData=='Metadata'",
                dataTableOutput("buildinMetaData")
              ),
              conditionalPanel(
                condition = "input.buildinShowData=='GEP'",
                dataTableOutput("buildinGEP")
              )
            )
          )
        ), # End tabPanel "Build-in data"
        
        tabPanel( 
          'Metadata in use', 
          sidebarLayout(        
            sidebarPanel(
              helpText("Select the dataset you want to use"),
              uiOutput("metadataselector")
            ),
            
            mainPanel(
              dataTableOutput("MetaDataInUseOut")
            )
          )
        ) # End tabPanel "Metadata in use"
      )  # End navbarPage "Type of data"/"Load data"
    ), # End tabPanel "Load data"
    
    tabPanel(
      'Results', 
      navbarPage(
        'Classification results', 
        tabPanel(
          'Estimated probabilities', 
          sidebarLayout(
            sidebarPanel(
              condition = "input.conditionedPanels == 'Classification results'",
              h3("Classification systems:"),
              checkboxGroupInput(
                "getClassifications",
                label = "Perform classifications:", 
                choices = c("BAGS",
                            "ABC / GCB (COO)" = "ABCGCB", 
                            "ABC / GCB / GCB-CC / GCB-CB (Extended COO)" = 
                              "ABCGCB2",
                            "Rituximab (R)", 
                            "Cyclophosphamide (C)",
                            "Doxorubicin (H)", 
                            "Vincristine (O)",
                            # "Dexamethasone", 
                            "Combined (CHO)",
                            "Melphalan" 
                ),
                selected = "BAGS"
              ),
              
              conditionalPanel(
                condition = paste(abc.cond, rtx.cond, cyc.cond, dox.cond,
                                  vin.cond, dex.cond, cho.cond, sep = "|"),
                h3("Classification options:")
              ),
              
              conditionalPanel(
                condition = abc.cond,
                sliderInput("nc.range",
                            "ABC/GCB, range of non classified:",
                            step = 0.01,
                            min = 0, max = 1, value = c(0.1, 0.9))
              ),

              conditionalPanel(
                condition = rtx.cond,
                checkboxInput(inputId = "HSCorrected", 
                              label = strong("Rituximap, human serum 
                                             corrected"),
                              value = TRUE),
                checkboxInput(inputId = "Lysis", 
                              label = strong("Rituximap, lysis based"), 
                              value = FALSE),
                sliderInput("Rituximab.range", 
                            "Rituximab, range of intermediate:",
                            step = 0.01,
                            min = 0, max = 1, value =  c(0.38, 0.54))
              ),
              
              conditionalPanel(
                condition = cyc.cond,
                sliderInput("Cyclophosphamide.range", 
                            "Cyclophosphamide, range of intermediate:", 
                            step = 0.01,
                            min = 0, max = 1, value = c(0.33,0.55))
              ),
              
              conditionalPanel(
                condition = dox.cond,
                sliderInput("Doxorubicin.range", 
                            "Doxorubicin, range of intermediate:", 
                            step = 0.01,
                            min = 0, max = 1, value = c(0.14, 0.9))
              ),
              
              conditionalPanel(
                condition = vin.cond,
                sliderInput("Vincristine.range", 
                            "Vincristine, range of intermediate:",
                            step = 0.01,
                            min = 0, max = 1, value =  c(0.46, 0.62))
              ),
              
              conditionalPanel(
                condition = dex.cond,
                sliderInput("Dexamethasone.range", 
                            "Dexamethasone, range of intermediate:", 
                            step = 0.01,
                            min = 0, max = 1, value = c(0.25, 0.75))
              ),

              conditionalPanel(
                condition = cho.cond,
                sliderInput("Combined.range", 
                            "Combined CHO, range of intermediate:", 
                            step = 0.01,
                            min = 0, max = 1, value = c(0.09, 0.93))
              ),
              
              br(),
              conditionalPanel(
                condition = paste(abc.cond, rtx.cond, cyc.cond, dox.cond,
                                  vin.cond, dex.cond, cho.cond, sep = "|"),
                helpText("Click to reset ranges to default:"),
                actionButton("resetButton",
                             HTML(paste(icon("repeat", lib = "glyphicon"), 
                                                      "Reset")))
              ),
              
              br(),
              tags$hr(), 
              downloadButton('downloadData', 'Download classification results')
            ), # End sidebarPanel
            mainPanel(
              shinyalert("shinyalertResults", click.hide = FALSE),
              dataTableOutput("results")
            )   # End mainPanel 
          )   # End sidebarLayout
        ),  # End Estimated probabilites tabPanel
        
        tabPanel( 
          'Patient summaries', 
          sidebarLayout( 
            sidebarPanel( 
              uiOutput("patientSummaryIPI"),
              uiOutput("patientSummarySelect"),
              hr(),
              jscolorInput("jscolorInputPS",
                           label = 
                             strong("Select colours used for the summaries:"),
                           value = "#9199D1"),
              br(),
              br(),
              actionButton(inputId = "SelectColourPS", 
                           HTML(paste(icon("chevron-down", lib = "glyphicon"), 
                                      "Add colour"))),
              br(),
              uiOutput("SelectedColoursPS"),
              hr(),
              sliderInput("surv.read", 
                          label = "Survival at (years)",
                          step = 3/12, min = 0, max = 7, value =  3)
            ),
            mainPanel(
              shinyalert("shinyalertPatientSummaries", click.hide = FALSE),
              uiOutput("patientSummaries"),
              plotOutput("patientSummaryPlot"),
              uiOutput("patientSummariesExplanation")
            )
          )
        )
      )
    ), # End tabPanel "Results"
    id = "navbarPanels",
    windowTitle = "hemaClass.org",
    inverse = TRUE,
    fluid = TRUE,
    footer  = ""
  ) # End navbarPage
)
