library(shiny)
#devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
#library(shinyIncubator)

shinyUI(
  navbarPage(title = "hemaClass", 
             tabPanel( 
               'Home',   
               navlistPanel(
                 "Information:",
                 tabPanel("hemaClass", h3("GEP Based Classification of DLBCL Patients"),
                          
                          p("This homepage is dedicated to classification of DLBCL", 
                            "patients according various classifications algorithms." )),
                 
                 tabPanel("News",
                          includeHTML("www/ex1.html")
                          ),
                 
                 tabPanel("Authors"),
                 tabPanel("Citation"),
                 tabPanel("Papers"),
                 
                 "-----",
                 tabPanel("Disclaimer")
               )),
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
                         
                         actionButton("buildreferenceButton", "Build the reference"),
                         
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
                           list("LLMPP CHOP" = "LLMPPCHOP",  
                                "LLMPP R-CHOP" = "LLMPPRCHOP", 
                                "IDRC" = "IDRC", 
                                "MDFCI" = "MDFCI"
                           )),
                         tags$hr() 
                       ),
                       conditionalPanel(
                         #condition = "input.ChooseMethod != 'blah'",
                         condition = "output.showNormButton!=0",
                         helpText("Normalise the files according to the chosen reference"),
                         actionButton("normalizeButton", "Normalize files", icon = icon("refresh"))
                       ),
                       
                       conditionalPanel(
                         condition = "input.ChooseMethod == 'build'",
                         tags$hr() ,
                         helpText("Download reference to save time on next run"),
                         downloadButton('downloadReference', 'Download reference for later use')      
                       )), 
                     mainPanel(
                       #helpText("The instructions below will aid you through the normalization process"), 
                       shinyalert("shinyalertUploadCel", click.hide = FALSE),
                       shinyalert("shinyalertUploadCelSucces", auto.close.after = 10),
                       shinyalert("shinyalertSelectReferenceSucess", auto.close.after = 10),
                       shinyalert("shinyalertSelectReference", click.hide = FALSE),
                       shinyalert("shinyalertNormalizationSuccess", click.hide = FALSE),
                      
                       conditionalPanel(
                         condition = "output.showErrorprints!=0",
                       verbatimTextOutput("start"), 
                       verbatimTextOutput("start2"), 
                       verbatimTextOutput("showNormButton"),
                       verbatimTextOutput("showErrorprints"))
                     )
                     
                   )), 
                 
                 
                 
                 navbarMenu( 
                   'Meta data', 
                   tabPanel('Upload meta data', 
                            sidebarPanel(
                              h4("Upload file containing meta data"),
                              
                              fileInput("usrMeta", "Please upload the file storing the metadata", 
                                        accept = "", multiple = FALSE)
                              
                            ), 
                            mainPanel(
                              shinyalert("shinyalertUploadMeta", click.hide = FALSE),
                              
                              helpText("The Output below shows how the data is read.")
                              
                            )
                            
                   ), 
                   tabPanel('Input manually', 
                            sidebarLayout( 
                              sidebarPanel(
                                helpText("You may also calculate IPI from clinical features."),
                                checkboxInput(inputId = "IPIcalc",
                                              label = "Calculate IPI", value = FALSE),
                                conditionalPanel(
                                  condition = "input.IPIcalc",
                                  helpText("You may change the settings for the IPI calculation."),
                                  numericInput(inputId = "AGE.cut", label = "Get point when age > x", 60),
                                  numericInput(inputId = "ECOG.cut", label = "Get point when ECOG > x", 1),
                                  numericInput(inputId = "LDH.cut", label = "Get point when LDH > x", 1),                                  
                                  numericInput(inputId = "N.Extra.Nodal.cut", label = "Get point when number of extra nodal sites > x", 1),
                                  numericInput(inputId = "Stage.cut", label = "Get point when Stage > x", 2)
                                  
                                )
                              ), 
                              mainPanel(
                                
                                shinyalert("shinyalertInputMeta", click.hide = FALSE),
                                
                                conditionalPanel(
                                  condition = "input.IPIcalc",
                                  helpText("Input the clinical features for each patien below."),
                                  div(class="well container-fluid",
                                      hotable("hotableClinical")),
                                  helpText("The calculated IPI values are shown below.")
                                ),
                                conditionalPanel(
                                  condition = "!input.IPIcalc",
                                  helpText("Input the IPI scores for each patient below.")
                                ),
                                div(class="well container-fluid",
                                    hotable("hotableIPI"))
                              )) 
                   ) 
                 ),
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
                                          choices = c("BAGS", "ABCGCB", "Cyclophosphamide", "Doxorubicin", "Vincristine", "Combined"),
                                          selected = c("BAGS")),
                       
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('ABCGCB') != -1",
                         h4("ABC/GCB options:"),
                         sliderInput("nc.range", "ABC/GCB, range of non classified:", step = 0.01,
                                     min = 0, max = 1, value = c(0.1,0.9))
                       ),
                       
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Cyclophosphamide') != -1",
                         h4("Cyclophosphamide options:"),
                         sliderInput("Cyclophosphamide.range", "Cyclophosphamide, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value = c(0.46,0.67))
                       ),
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Doxorubicin') != -1",
                         h4("Doxorubicin options:"),
                         sliderInput("Doxorubicin.range", "Doxorubicin, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value = c(0.1, 0.86))
                       ),
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Vincristine') != -1",
                         h4("Vincristine options:"),
                         sliderInput("Vincristine.range", "Vincristine, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value =  c(0.38, 0.54))
                       ),
                       
                       conditionalPanel(
                         condition = "input.getClassifications.indexOf('Combined') != -1",
                         h4("Combined options:"),
                         sliderInput("Combined.range", "Combined, range of intermediate:", step = 0.01,
                                     min = 0, max = 1, value = c(0.07,0.91))
                       ),
                       
                       br(),br(),
                       downloadButton('downloadData', 'Download classification results')
                     ), 
                     mainPanel(shinyalert("shinyalertResults", click.hide = FALSE),
                               dataTableOutput("results")) 
                   )),
                 tabPanel( 
                   'Patient summaries'
                 ),
                 tabPanel( 
                   'Prognostics'
                 )
               )), 
             
             
             
             navbarMenu( 
               'Survival Analysis', 
               tabPanel('Info', sidebarLayout( 
                 sidebarPanel('one panel'), 
                 mainPanel() 
               )), 
               tabPanel('Descriptive', navbarPage(
                 'Descriptive',
                 tabPanel( 'Kaplan-Meier', sidebarLayout( 
                   sidebarPanel('one panel'), 
                   mainPanel() 
                 )),
                 tabPanel('Cumulative incidens',sidebarLayout( 
                   sidebarPanel('one panel'), 
                   mainPanel() 
                 )) 
               )),
               tabPanel('Cox regression', navbarPage(
                 'Proportional hazards',
                 tabPanel( 'Analysis', sidebarLayout( 
                   sidebarPanel('one panel'), 
                   mainPanel() 
                 )),
                 tabPanel('Model control',sidebarLayout( 
                   sidebarPanel('one panel'), 
                   mainPanel() 
                 )) 
               ))),
               br(),br(),br(),br(),br(),br(),br(),br(),
               id = "navbarPanels",
             windowTitle = "hemaClass",
             inverse = TRUE,
             fluid = TRUE,
             footer  = ""
  )
)