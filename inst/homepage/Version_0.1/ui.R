library(shiny)
#devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
library(RLumShiny)
#library(shinyIncubator)

shinyUI(
  navbarPage(theme = "bootstrap.css",
             title = "hemaClass",
             tabPanel( 
               'Home',   
               sidebarLayout(
                sidebarPanel(
                  navlistPanel(
                    "Information:",
                    tabPanel("hemaClass"),    
                    tabPanel("News"),
                    tabPanel("Help"),
                    tabPanel("Publications"),
                    tabPanel("About"),
                    well=FALSE,
                    id="nlp",
                    widths=c(12,1)),
                  width=4),
                mainPanel(
                  htmlOutput("mpContent")
                ))),
             
             tabPanel( 
               'Load data',
               navbarPage( 
                 
                 'Type of data', 
                 
                 tabPanel( 
                   'CEL files', 
                   sidebarLayout(        
                     sidebarPanel(
                       h4("Upload .CEL files"),
                       busyIndicator("Pre-processing .CEL files", wait = 1000),
                       fileInput("usrFiles", "Please choose your .CEL files to classify:", 
                                 accept = "", multiple = TRUE),
                       
                       tags$hr() ,
                       conditionalPanel(
                         #condition = "input.ChooseMethod != 'blah'",
                         condition = "output.showNormMethods!=0",
                         
                         h5("RMA Pre-processing"),
                         selectInput(
                           "ChooseMethod", "Choose method",
                           list("Please select" = "blah",
                                "Use build in reference" = "standardReference",  
                                "Build a new reference" = "build", 
                                "Upload a reference" = "upload",
                                "Cohort based RMA" = "RMA"
                           )),
                         
                         #                    conditionalPanel(
                         #                      condition = "input.ChooseMethod == 'blah'",
                         #                      
                         #                      
                         #                      tags$hr(),  
                         #                      tags$hr(),  
                         #                      tags$hr()  
                         #                    ),
                         
                         conditionalPanel(
                           condition = "input.ChooseMethod == 'build'",
                           #helpText("In order to build a laboratory specific reference you need to upload .CEL files"),
                           
                           fileInput("refFiles", "In order to build a laboratory specific reference you need to upload .CEL files:", 
                                     accept = "", multiple = TRUE),
                           conditionalPanel(
                             condition = "output.showBuildRefButton!=0",
                             actionButton("buildreferenceButton", "Build the reference"),
                             tags$hr(),
                             conditionalPanel(
                               condition = "output.showDownloadRefButton!=0",
                               helpText("Download reference to save time on next run"),
                               downloadButton('downloadReference', 'Download reference for later use')    
                             )
                           )
                         ),
                         tags$hr()  
                       ),
                       
                       conditionalPanel(
                         condition = "input.ChooseMethod == 'upload'",
                         #helpText("Upload the file containing the reference"),
                         
                         fileInput("refUpload", "Upload the file containing the reference:", 
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
                         #condition = "input.ChooseMethod != 'blah'",
                         condition = "output.showNormButton!=0",
                         helpText("Normalize the files according to the chosen reference"),
                         actionButton("normalizeButton", "Normalize files", icon = icon("refresh"))
                       )
                       
                       
                     ), 
                     mainPanel(
                       shinyalert("shinyalertUploadCel", click.hide = FALSE),
                       shinyalert("shinyalertSelectReference", click.hide = FALSE),
                       #helpText("The instructions below will aid you through the normalization process"), 
                       shinyalert("shinyalertUploadCelSucces", auto.close.after = 10),
                       shinyalert("shinyalertSelectReferenceSucess", auto.close.after = 10),
                       shinyalert("shinyalertNormalizationSuccess", click.hide = FALSE, auto.close.after = 10),
                       
                       
                       dataTableOutput("normalizedData"),
                       
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
                     
                   )), 
                 
                 
                 
                 navbarMenu( 
                   'Metadata', 
                   tabPanel('Upload metadata', 
                            sidebarPanel(
                              h4("Upload file containing metadata"),
                              busyIndicator("Processing the metadata", wait = 1000),
                              fileInput("usrMeta", "Please upload the file storing the metadata", 
                                        accept = "", multiple = FALSE),
                              
                              conditionalPanel(
                                condition = "output.showReadMetaMethods=='txt'",
                                
                                helpText("Does the file contain a header"),
                                checkboxInput("ExttxtHeader", label = "header", value = TRUE),
                                
                                selectInput(
                                  "ExttxtSep", "Choose separater",
                                  list("Tabulate" = "\t",
                                       "Semicolon" = ";",  
                                       "comma" = ",", 
                                       "Other" = "Other"
                                  )),
                                conditionalPanel(
                                  condition = "input.ExttxtSep=='Other'", 
                                  textInput(inputId = "ExttxtSepOther", "Type in the separator", value = ";")
                                )
                              ),
                              conditionalPanel(
                                condition = "output.showReadMetaMethods=='xls'|| output.showReadMetaMethods=='xlsx'",
                                
                                textInput(inputId = "ExtXLSsheet", "The sheet you want to read", 1)
                                
                              ),
                              uiOutput("MetaUploadCelFileNames")
                            ), 
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
                   ),
                   tabPanel('Input manually', 
                            sidebarLayout( 
                              sidebarPanel(
                                helpText("You may also calculate IPI from clinical features."),
                                checkboxInput(inputId = "IPIcalc",
                                              label = "Calculate IPI", value = FALSE),
                                
                                helpText("Additional columns"),
                                
                                select2Input("Additionalcolumns","Type in the names.",
                                             choices=c("Age","LDH"),
                                             selected=NULL),
                                
                                conditionalPanel(
                                  condition = "input.IPIcalc",
                                  helpText("You may change the settings for the IPI calculation."),
                                  numericInput(inputId = "AGE.cut", label = "Get point when age > x", 60),
                                  numericInput(inputId = "ECOG.cut", label = "Get point when ECOG > x", 1),
                                  numericInput(inputId = "LDH.cut", label = "Get point when LDH > x", 1),                                  
                                  numericInput(inputId = "N.Extra.Nodal.cut", label = "Get point when number of extra nodal sites > x", 1),
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
                                  #div(class="well container-fluid",
                                  #    hotable("hotableClinical")),
                                ),
                                conditionalPanel(
                                  condition = "!input.IPIcalc",
                                  helpText("Input the IPI scores for each patient below.")
                                ),
                                #div(class="well container-fluid",
                                #    hotable("hotableIPI")),
                                div(class="well container-fluid",
                                    hotable("hotableMetadataManual"))
                              )
                            ) 
                   ) 
                 ),
                 
                 
                 tabPanel( 
                   'Build-in data', 
                   sidebarLayout(        
                     sidebarPanel(
                       busyIndicator("Loading data", wait = 1000),
                       helpText("Select the dataset you want to analyse"),
                       uiOutput("buildindataselector"),
                       
                       radioButtons("buildinShowData", "Show", 
                                    choices = list("Metadata" = "Metadata",
                                                   "Gene Expression" = "GEP"),
                                    selected = "GEP")
                       
                       #                        checkboxInput("buildinShowMetadata",
                       #                                      label = "Show metadata:",  
                       #                                      value =  TRUE),
                       #                        checkboxInput("buildinShowGEP",
                       #                                      label = "Show GEP:",  
                       #                                      value =  FALSE)
                     ),
                     
                     mainPanel(
                       conditionalPanel(
                         condition = "input.buildinShowData=='Metadata'",
                         dataTableOutput("buildinMetaData")
                       ),
                       conditionalPanel(
                         condition = "input.buildinShowData=='GEP'",
                         dataTableOutput("buildinGEP")
                       ))
                   )),
                 
                 
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
                 ),
                 
                 #                  tabPanel('Pre-processed CEL files',
                 #                           sidebarLayout( 
                 #                             sidebarPanel("Data"),
                 #                             mainPanel(
                 #                               dataTableOutput("normalizedData")
                 #                             ))
                 #                  ),
                 br(),br(),br(),br(),br(),br(),br(),br())
             ),
             
             
             
             tabPanel( 
               'Results', 
               
               navbarPage( 
                 
                 'Classification results', 
                 
                 tabPanel( 
                   'Estimated probabilities', 
                   # headerPanel("The resulting classification"),
                   sidebarLayout(       
                     sidebarPanel(
                       condition = "input.conditionedPanels == 'Classification results'",
                       h4("Classification systems:"),
                       checkboxGroupInput("getClassifications", ,
                                          label = "Perform classifications:", 
                                          choices = c("BAGS", "ABCGCB", "ABCGCB2", "Rituximab (R)", "Cyclophosphamide (C)", "Doxorubicin (H)", "Vincristine (O)", "Dexamethasone (P)", "Combined (CHO)", "Melphalan"),
                                          selected = c("BAGS")),
                       
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('ABCGCB') != -1",
                         h4("ABC/GCB options:"),
                         sliderInput("nc.range", "ABC/GCB, range of non classified:", step = 0.01,
                                     min = 0, max = 1, value = c(0.1,0.9))
                       ),
                       
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Rituximab (R)') != -1",
                         h4("Rituximab options:"),
                         checkboxInput(inputId = "HSCorrected", "Human serum corrected", value = TRUE),
                         checkboxInput(inputId = "Lysis", "Lysis based", value = FALSE),
                         sliderInput("Rituximab.range", "Rituximab, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value =  c(0.38, 0.54))
                       ),
                       
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Cyclophosphamide (C)') != -1",
                         h4("Cyclophosphamide options:"),
                         sliderInput("Cyclophosphamide.range", "Cyclophosphamide, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value = c(0.46,0.67))
                       ),
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Doxorubicin (H)') != -1",
                         h4("Doxorubicin options:"),
                         sliderInput("Doxorubicin.range", "Doxorubicin, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value = c(0.1, 0.86))
                       ),
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Vincristine (O)') != -1",
                         h4("Vincristine options:"),
                         sliderInput("Vincristine.range", "Vincristine, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value =  c(0.38, 0.54))
                       ),
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Dexamethasone (P)') != -1",
                         h4("Dexamethasone options:"),
                         sliderInput("Dexamethasone.range", "Dexamethasone, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value = c(0.25,0.75))
                       ),
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Combined (CHO)') != -1",
                         h4("Combined CHO options:"),
                         sliderInput("Combined.range", "Combined CHO, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value = c(0.07,0.91))
                       ),
                       
                       
                       br(),br(),
                       downloadButton('downloadData', 'Download classification results')
                     ), 
                     mainPanel(shinyalert("shinyalertResults", click.hide = FALSE),
                               dataTableOutput("results")) 
                   )),
                 tabPanel( 
                   'Patient summaries', 
                   sidebarLayout( 
                     sidebarPanel( 
                       uiOutput("patientSummaryIPI"),
                       uiOutput("patientSummarySelect"),
                       tags$hr() ,
                       # uiOutput("patientSumCols"),
                       helpText("Select the colors used for the predicted survival curves"),
                       jscolorInput("jscolorInputPS", value = "#333333"),
                       br(),br(),
                       actionButton(inputId = "SelectColourPS", "Add the color"),
                       br(),
                       uiOutput("SelectedColoursPS")
                       #select2Input("SelectedColoursPS", label = "The selected colours", selected = "333333", type = "input") 
                     ),
                     mainPanel(
                       plotOutput("patientSummaryPlot"),
                       uiOutput("patientSummaries")
                     )
                   )
                 )
               )), 
             
             
             
#              navbarMenu( 
#                'Survival Analysis', 
#                tabPanel('Info', sidebarLayout( 
#                  sidebarPanel('one panel'), 
#                  mainPanel() 
#                )), 
#                tabPanel('Descriptive', navbarPage(
#                  'Descriptive',
#                  tabPanel( 'Kaplan-Meier', sidebarLayout( 
#                    sidebarPanel( 
#                      uiOutput("KMSurvival"),
#                      
#                      textInput("geneKM", "Which gene should be analysed (e.g. MS4A1)", value = "Gene"),
#                      uiOutput("probeselector")
#                    ),
#                    mainPanel() 
#                  )),
#                  tabPanel('Cumulative incidens',sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  )) 
#                )),
#                tabPanel('Cox regression', navbarPage(
#                  'Proportional hazards',
#                  tabPanel( 'Analysis', sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  )),
#                  tabPanel('Model control',sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  )) 
#                ))),
#              navbarMenu( 
#                'Differentially expressed', 
#                tabPanel('Info', sidebarLayout( 
#                  sidebarPanel('one panel'), 
#                  mainPanel() 
#                )), 
#                tabPanel('Descriptive', navbarPage(
#                  'Descriptive',
#                  tabPanel( 'Cross tabulation', sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  )),
#                  tabPanel('Cumulative incidens',sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  )) 
#                )),
#                tabPanel('DE Methods', navbarPage(
#                  'Analysis',
#                  tabPanel("Welch's t test",sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  )),
#                  tabPanel( 'Linear regression', sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  )),
#                  tabPanel( 'Linear mixed-effects model', sidebarLayout( 
#                    sidebarPanel('one panel'), 
#                    mainPanel() 
#                  ))
#                ))),
             br(),br(),br(),br(),br(),br(),br(),br(),
             id = "navbarPanels",
             windowTitle = "hemaClass",
             inverse = TRUE,
             fluid = TRUE,
             footer  = ""
  )
)