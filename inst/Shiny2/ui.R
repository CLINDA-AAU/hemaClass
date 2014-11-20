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
                 tabPanel("News"),
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
                         condition = "input.ChooseMethod != 'blah'",
                         helpText("Normalise the files according to the chosen reference"),
                         actionButton("normalizeButton", "Normalize files", icon = icon("refresh"))
                       ),
                       
                       conditionalPanel(
                         condition = "input.ChooseMethod == 'build'",
                         tags$hr() ,
                         helpText("Download reference to save time"),
                         downloadButton('downloadReference', 'Download reference for later use')      
                       )), 
                     mainPanel(
                       helpText("The instructions below will aid you through the normalization process"), 
                       verbatimTextOutput("start") 
                     )
                     
                   )), 
                 
                 
                            
                 navbarMenu( 
                   'Meta data', 
                   tabPanel('Upload meta data', 
                            sidebarPanel(
                              h4("Upload file"),
                              
                              fileInput("usrMeta", "Please upload the file storing the metadata", 
                                        accept = "", multiple = FALSE)
                              
                            ), 
                            mainPanel(
                              helpText("The Output below shows how the data is read.")
                            )
                            
                   ), 
                   tabPanel('Input manually', 
                            sidebarLayout( 
                              sidebarPanel(
                                helpText("You may also calculate IPI from clinical features."),
                                checkboxInput(inputId = "IPIcalc",
                                              label = "Calculate IPI", value = FALSE)
                                ), 
                              mainPanel(
                                conditionalPanel(
                                  condition = "input.IPIcalc",
                                  div(class="well container-fluid",
                                      hotable("hotableClinical"))
                                ),
                                div(class="well container-fluid",
                                    hotable("hotableIPI"))
                              )) 
                   ) 
                 ),
                 br(),br(),br(),br(),br(),br(),br(),br())
             ),
             
             
             tabPanel( 
               'Classification results', 
               # headerPanel("The resulting classification"),
               sidebarLayout(       
                 sidebarPanel(
                   condition = "input.conditionedPanels == 'Classification results'",
                   h4("Classification systems:"),
                   checkboxGroupInput("getClassifications", 
                                      label = "Perform classifications:", 
                                      choices = c("BAGS", "ABCGCB", "Cyclophosphamide", "Doxorubicin", "Vincristine", "Combined"),
                                      selected = c("BAGS", "ABCGCB","Cyclophosphamide", "Doxorubicin", "Vincristine", "Combined")),
                   
                   
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
                 mainPanel(dataTableOutput("results")) 
               )), 
             
             
             
             tabPanel("test",
                      navbarPage( 
                        
                        'testapp', 
                        
                        tabPanel( 
                          'Other Panel', 
                          sidebarLayout( 
                            sidebarPanel('test'), 
                            mainPanel() 
                          )), 
                        
                        navbarMenu( 
                          'menu', 
                          tabPanel('test_panel_1', sidebarLayout( 
                            sidebarPanel('one panel'), 
                            mainPanel() 
                          )), 
                          tabPanel('test_panel_2', sidebarLayout( 
                            sidebarPanel('another panel'), 
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