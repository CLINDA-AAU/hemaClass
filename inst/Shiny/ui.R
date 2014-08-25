library(shiny)
#library(shinyIncubator)

shinyUI(
  pageWithSidebar(
    headerPanel("Hematological GEP Classification Systems"),
    
    sidebarPanel(
      h4("Upload .CEL files"),
      
      fileInput("refFiles", "Please choose your .CEL files to build a reference:", 
                accept = "", multiple = TRUE),
      actionButton("buildreferenceButton", "Build the reference"),
      
      fileInput("usrFiles", "Please choose your .CEL files to classify:", 
                accept = "", multiple = TRUE),
      actionButton("normalizeButton", "Normalize files"),
      
      conditionalPanel(
        condition = "input.conditionedPanels == 'Cross tabulation'",
      
      selectInput(
        "xtablechoose1", "Cross tabulator row",
        list("ABCGCB" = "ABCGCB", 
             "BAGS" = "BAGS",  
             "Cyclophosphamide" = "CycClass", 
             "Doxorubicin" = "DoxClass", 
             "Vincristine" = "VinClass", 
             "Combined" = "CombClass"
        )),
        
        selectInput(
          "xtablechoose2", "Cross tabulator column",
          list("ABCGCB" = "ABCGCB", 
               "BAGS" = "BAGS",  
               "Cyclophosphamide" = "CycClass", 
               "Doxorubicin" = "DoxClass", 
               "Vincristine" = "VinClass", 
               "Combined" = "CombClass"
          ))
      ),
      
      
      
      conditionalPanel(
        condition = "input.conditionedPanels == 'Classification results'",
        br(),br(),h4("Classification systems:"),
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

      conditionalPanel(
        condition = "input.conditionedPanels == 'Density plot'",
        br(),br(),br(),h4("Plot options:"),
        sliderInput("xrange", "Plot x-axis range:", step = 0.5,
                    min = -10, max = 30, value = c(-4,17)),
        sliderInput("yrange", "Plot y-axis range:", step = 0.01,
                    min = 0, max = 2, value = c(0,0.2))
      )
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Start", verbatimTextOutput("start")),
        tabPanel("Classification results",  dataTableOutput("results")),
        tabPanel("Cross tabulation",  tableOutput("xtabs")),
        tabPanel("Density plot", plotOutput("plot")),
        tabPanel("Expression matrix", tableOutput("expression.matrix")),
        #tabPanel("usrFiles", tableOutput("usrFiles")),
        #tabPanel("print stuff", verbatimTextOutput("stuff")),
        id = "conditionedPanels"
      ),
      ### show timer
      conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
                       id='progressIndicator',
                       "HI I'M IN PROGRESS",
                       div(id='progress',includeHTML("timer.js"))
      ),
      tags$head(tags$style(type="text/css",
                           '#progressIndicator {',
                           '  position: fixed; top: 8px; right: 8px; width: 200px; height: 50px;',
                           '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
                           '}'
      ))
    )
  )
)