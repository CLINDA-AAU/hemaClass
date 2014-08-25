#install.packages("devtools")
#devtools::install_github("shiny", "rstudio")
#devtools::install_github("shiny-incubator", "rstudio")
library(shiny)
library(hemaClass)
# Changing maximum file size
options(shiny.maxRequestSize = 30*1024^2)
cat("done.\n")

#
# Run the shiny server
#

shinyServer(function(input, output, session) {
  # Initialzing the normalized.data and results object
  normalized.data <- NULL
  results <- list()
    
  createData <- reactive({
    # Take a dependency on input$normalizeButton by calling it
    input$normalizeButton
    
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      normalized.data <<- rmanormalizer(input$usrFiles)
      colnames(normalized.data) <<- gsub("\\.CEL$", "", input$usrFiles$name)
    })
  })
  
  # Function to that calls the classification procedures
  classify <- reactive({
    results <<- list()
    createData()  # Create or update data if necessary

    if (is.null(normalized.data)) {
      return(NULL)
    }
    
    results$files <<- colnames(normalized.data)
    
    if ("ABCGCB" %in% input$getClassifications) {
      abcgcb <- qNormCenterABCGCB(normalized.data)
      abcgcb <- ABCGCBClassify(abcgcb, NC.range = input$nc.range)
      results$"ProbOfABC" <<- abcgcb$"ProbOfABC"
      results$"ABCGCB" <<- abcgcb$"ABCGCB"
    } else {
      results <<- results[!(names(results) %in% c("ProbOfABC", "ABCGCB"))]
    }

    
    if ("BAGS" %in% input$getClassifications) {
      bags <- qNormCenterBAGS(normalized.data)
      bags <- BAGSclassifier((bags), cut.spec=0 )
      results$ProbOfBAGS <<- bags$prob
      results$BAGS <<- bags$class
    } else {
      results <<- results[!(names(results) %in% c("ProbOfBAGS", "BAGS"))]
    }
    
    av.drugs <- c("Cyclophosphamide", "Doxorubicin", "Vincristine") 
    drugs <<- input$getClassifications 
    drugs <- drugs[drugs %in% c(av.drugs, "Combined")]
    if (length(intersect(av.drugs,input$getClassifications)) > 0) {
      
      cut <- list(Cyclophosphamide = input$Cyclophosphamide.range,
                 Doxorubicin = input$Doxorubicin.range,
                 Vincristine = input$Vincristine.range,
                 Combined    = input$Combined.range)
      
      drug.norm <- qNormCenterDrug(normalized.data)
      CHO <- Resistanceclassifier(drug.norm, drugs=drugs, cut = cut)
      
      if ("Cyclophosphamide" %in% input$getClassifications) {
        results$CycProb  <<- CHO$prob[,"Cyclophosphamide"]
        results$CycClass <<- CHO$class[,"Cyclophosphamide"]
      } else {
        results <<- results[!(names(results) %in% c("CycProb", "CycClass"))]
      }
      
      if ("Doxorubicin" %in% input$getClassifications) {
        results$DoxProb  <<- CHO$prob[,"Doxorubicin"]
        results$DoxClass <<- CHO$class[,"Doxorubicin"]
      } else {
        results <<- results[!(names(results) %in% c("DoxProb", "DoxClass"))]
      }
      if ("Vincristine" %in% input$getClassifications) {
        results$VinProb  <<- CHO$prob[,"Vincristine"]
        results$VinClass <<- CHO$class[,"Vincristine"]
      } else {
        results <<- results[!(names(results) %in% c("VinProb", "VinProb"))]
      }
      if ("Combined" %in% input$getClassifications & 
            length(drugs) > 2) {
        results$CombProb  <<- CHO$prob[,"Combined"]
        results$CombClass <<- CHO$class[,"Combined"]
      } else {
        results <<- results[!(names(results) %in% c("CombProb", "CombClass"))]
      }
    } else {
      results <<- results[!(names(results) %in% c("CycProb", "CycClass", "DoxProb", "DoxClass",
                                                  "VinProb", "VinProb","CombProb", "CombClass"))]
    }
    
    results <<- base::as.data.frame(results, row.names = NULL)
    rownames(results) <<- NULL
  })
  
  
  
  
  observe({  
    
    classifers.ch <- input$getClassifications
    if(length(classifers.ch) > 0){
      l<- list("ABCGCB" = "ABCGCB", 
               "BAGS" = "BAGS",  
               "Cyclophosphamide" = "CycClass", 
               "Doxorubicin" = "DoxClass", 
               "Vincristine" = "VinClass", 
               "Combined" = "CombClass"
      )
      l <- l[classifers.ch]
      
      updateSelectInput(session, "xtablechoose1",
                        choices=l,
                        select=classifers.ch[1])
      
      a <- ifelse(length(classifers.ch) > 1, 2, 1)
      updateSelectInput(session, "xtablechoose2",
                        choices=l,
                        select=classifers.ch[a])
      
      
   }
  })
  
  #
  # Generate output
  #
  
  # The start tab
  output$start <- renderPrint({    
    # Take dependency on input$normalizeButton by calling it
    input$normalizeButton
    
    if (is.null(input$usrFiles))
      return(cat("No .CEL files chosen!",
                 "\nPlease press 'Choose files' and select your CEL files"))
    
    if (is.null(normalized.data) & input$normalizeButton == 0)
      return(cat("Press 'normalize files' to start the RMA normalization."))
    
    # Normalize the data
    createData() # Create or update data if necessary
    cat("The normalization was successful!")
  })
  
  # Generate results
  output$results <- renderDataTable({ 
    classify() # Classify
    
    if (length(results) == 0)
      return(NULL)
    results2 <- results
    if( length(names(results)) >0){
      for(i in names(results)){
      if(class(results[,i]) == "numeric")
        results2[,i] <- round(results2[,i], 3)
      }
      return(results2)
      }else{
      return(NULL)
    }
    
    
  })
  
  
  
  output$xtabs <- renderTable({ 
    classify() # Classify
    
  #  if(length(results) == 0){
  #    return(NULL)
      
  #  }else{
    if( length(names(results)) >0){
           xtabs <- table(results[, input$xtablechoose1], results[, input$xtablechoose2])
           return(xtabs)
         }else{
           return(NULL)
         }
    # }
  })
  
  # Generate density plot
  output$plot <- renderPlot({
    if (is.null(normalized.data))
      return(NULL)
    
    createData() # Create or update data if necessary

    plot(1, type = "n", axes = FALSE,
         xlim = input$xrange, ylim = input$yrange,
         main = "Density of normalized CEL files", 
         ylab = "Density", xlab = "Expression level")
    grid(); axis(1); axis(2)
    for (i in 1:ncol(normalized.data)) {
      lines(density(normalized.data[,i]), 
            col = colorRampPalette(c("Red", "Blue"))(ncol(normalized.data))[i])
    }
  })
  
  output$expression.matrix <- renderTable({
    if (is.null(normalized.data))
      return(NULL)

    createData() # Create or update data if necessary
    
    return(as.data.frame(normalized.data[1:100, , drop = FALSE]))
  })
  
  
  # List all input
  output$stuff <- renderPrint({
    cat("> str(results)\n")
    str(results)
    cat("> getwd()\n")
    getwd()
    cat("> names(input)\n")
    names(input)
  })
  
  # Render table of uploaded files
  output$usrFiles <- renderTable({  
    # input$celFiles will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    usrFiles <- input$usrFiles
    
    if (is.null(usrFiles)) 
      return(NULL)

    return(usrFiles)    
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = paste("HemaClass-", Sys.Date(), ".txt", sep = ""),
    content = function(file) {
      write.table(results, file, sep = "\t", quote = FALSE)
    }
  )

})
