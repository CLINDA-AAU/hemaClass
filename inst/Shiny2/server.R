#install.packages("devtools")
#devtools::install_github("shiny", "rstudio")
#devtools::install_github("shiny-incubator", "rstudio")
library(shiny)
library(hemaClass)
#devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
# Changing maximum file size
options(shiny.maxRequestSize = 30*1024^2)
cat("done.\n")

#
# Run the shiny server
#

shinyServer(function(input, output, session) {
  # Initialzing the normalized.data and results object
  normalized.data <- NULL
  normalized.data.RMA <- NULL
  user.reference  <- NULL
  user.reference.affy <- NULL
  results <- list()
    
  # hotable
  output$hotableIPI <- renderHotable({ 
    fileInfo <- input$usrFiles
    if(!is.null(fileInfo$name)){
      if(input$IPIcalc){
        data <- hot.to.df(input$hotableIPI)
        
        if(!is.null(hot.to.df(input$hotableClinical))){
          data$IPI <- IPIreactive()        
        }
        data
      }else{
        
        data <- matrix(NaN, ncol =  2, nrow = length(fileInfo$name), 
                       dimnames = list(fileInfo$name, c("Patient","IPI")))
        data <- as.data.frame(data)
        data$Patient <- fileInfo$name
        data
      }
    }else{
      data <- matrix(NaN, ncol =  2, nrow = length(fileInfo$name), 
                     dimnames = list(fileInfo$name, c("Patient","IPI")))
      as.data.frame(data)
    }
  }, readOnly = FALSE)
  
  observe({    
    df <- hot.to.df(input$hotableIPI)
    print(df)
  })
  
  output$hotableClinical <- renderHotable({  
    fileInfo <- input$usrFiles
    if(!is.null(fileInfo$name)){
      if(!exists("input$hotableClinical")){        
        data <- matrix(NaN, ncol =  6, nrow = length(fileInfo$name), 
                       dimnames = list(fileInfo$name, c("Patient", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage")))
        data <- as.data.frame(data)
        data$Patient <- fileInfo$name
        data
      }else{
        old.data <- hot.to.df(input$hotableClinical)
        data <- matrix(NaN, ncol =  6, nrow = length(fileInfo$name), 
                       dimnames = list(fileInfo$name, c("Patient", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage")))
        data <- as.data.frame(data)
        data$Patient <- fileInfo$name
        
        int <- intersect(old.data$Patient, data$Patient)
        
        if(length(int))
          data[int, ] <- old.data[int, ]
      }
    }else{
      data <- matrix(NaN, ncol =  6, nrow = length(fileInfo$name), 
                   dimnames = list(fileInfo$name, c("Patient", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage")))
      as.data.frame(data)
    }
  }, readOnly = FALSE)
  
  observe({
    df <- hot.to.df(input$hotableClinical)
    print(df)
  })
  
  IPIreactive <- reactive({   
    data <- hot.to.df(input$hotableClinical)
    a <- ifelse(data$Age            >  60, 1, 0)
    b <- ifelse(data$ECOG           >   1, 1, 0)
    c <- ifelse(data$N.Extra.Nodal  >=  2, 1, 0)
    d <- ifelse(data$Stage          >   2, 1, 0)
    e <- ifelse(data$LDH            >   1, 1, 0)
    rowSums(data.frame(a, b, c, d, e))
  })
  
 
  createReferenceAffy <- reactive({
    # Use isolate() to avoid dependency on input$refFiles
    isolate({
      fileInfo <- input$refFiles
      if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE)))
        stop("Not all chosen files are .CEL files.")
           
      user.reference.affy <<- readCelfiles(input$refFiles$datapath)
    })
  })
  
  createReference <- reactive({
    # Take a dependency on createReferenceAffy by calling it
    input$buildreferenceButton
    
    createReferenceAffy()
    
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      user.reference <<- rmaPreprocessing(user.reference.affy)
      user.reference$exprs <<- NULL
      user.reference$exprs.sc <<- NULL
    })
  })
  
  
  createUserAffy <- reactive({
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      user.affy <<- readCelfiles(input$usrFiles$datapath)
    })
  })
  

  createData <- reactive({
    # Take a dependency on input$normalizeButton by calling it
    input$normalizeButton
    
    createUserAffy()
    
    fileInfo <- input$usrFiles
    if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE)))
      stop("Not all chosen files are .CEL files.")
    
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      if(input$ChooseMethod == "standardReference")       
        ref <- switch(EXPR = input$ChooseReference,
         "LLMPPCHOP"  = readLLMPPCHOPreference(),
         "LLMPPRCHOP" = readLLMPPRCHOPreference(),
         "MDFCI"      = readMDFCIreference(),
         "IDRC"       = readIDRCreference())
      
      if(input$ChooseMethod == "build") 
        ref <- user.reference
      
      if(input$ChooseMethod == "upload") 
        ref <- readRDS(input$refUpload$datapath)
      
      if(input$ChooseMethod != "RMA"){ 
        normalized.data.RMA <<- rmaReference(user.affy, reference = ref)
      }else{
        normalized.data.RMA <<- rmaPreprocessing(user.affy)
      }
      colnames(normalized.data.RMA$exprs) <<- gsub("\\.CEL$", "", input$usrFiles$name)
      colnames(normalized.data.RMA$exprs.sc) <<- gsub("\\.CEL$", "", input$usrFiles$name)
      normalized.data <<- normalized.data.RMA$exprs.sc
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
      abcgcb <- ABCGCB(normalized.data, NC.range = input$nc.range)
      results$"ProbOfABC" <<- abcgcb$prob
      results$"ABCGCB" <<- abcgcb$class
    } else {
      results <<- results[!(names(results) %in% c("ProbOfABC", "ABCGCB"))]
    }

    
    if ("BAGS" %in% input$getClassifications) {
      bags <- BAGS(normalized.data, cut.spec=0)
      results$"ProbOfBAGS" <<- bags$prob
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
      
      CHO <- ResistanceClassifier(normalized.data, drugs=drugs, cut = cut)
      
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
    input$buildreferenceButton

    if (is.null(input$usrFiles))
      return(cat("No .CEL files chosen!",
                 "\nPlease press 'Choose files' and select the CEL files you want to classify"))
    
    if(input$ChooseMethod == "blah")
      return(cat("Please select a reference for RMA-preprocessing"))
    
    if (is.null(input$refFiles) & input$ChooseMethod == "build")
      return(cat("No .CEL files chosen for building the reference!",
                 "\nPlease press 'Choose files' and select the CEL files", 
                 "\nyou want to use for building the classifier"))
        
    if (is.null(user.reference) & input$buildreferenceButton == 0 & input$ChooseMethod == "build")
      return(cat("Press 'Build the reference' to start the RMA reference building."))
    
     if (input$ChooseMethod == "build" & is.null(user.reference) & input$normalizeButton == 0){
       createReferenceAffy()
       createReference()
       return(cat("The reference was successfully build!",
                  "\nPress 'normalize files' to start the RMA normalization."))
     }
    
    if (input$ChooseMethod == "build"){
      createReferenceAffy()
      createReference()
    } 
    
    if (input$ChooseMethod == "upload" & is.null(input$refUpload))
      return(cat("Please upload the reference"))
          
    
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
    if (is.null(normalized.data.RMA$exprs))
      return(NULL)
    
    createData() # Create or update data if necessary

    plot(1, type = "n", axes = FALSE,
         xlim = input$xrange, ylim = input$yrange,
         main = "Density of normalized CEL files", 
         ylab = "Density", xlab = "Expression level")
    grid(); axis(1); axis(2)
    for (i in 1:ncol(normalized.data.RMA$exprs)) {
      lines(density(normalized.data.RMA$exprs[,i]), 
            col = colorRampPalette(c("Red", "Blue"))(ncol(normalized.data.RMA$exprs))[i])
    }
  })
  
  output$expression.matrix <- renderTable({
    if (is.null(normalized.data.RMA))
      return(NULL)

    createData() # Create or update data if necessary
    
    return(as.data.frame(normalized.data.RMA$exprs[1:100, , drop = FALSE]))
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
  
  # Download handler for results
  output$downloadData <- downloadHandler(
    filename = paste("HemaClass-Classifications", Sys.Date(), ".txt", sep = ""),
    content = function(file) {
      write.table(results, file, sep = "\t", quote = FALSE)
    }
  )
  
  # Download handler for reference
  output$downloadReference <- downloadHandler(
    filename = paste("HemaClass-Reference", Sys.Date(), ".rds", sep = ""),
    content = function(file) {
      saveRDS(object = user.reference, file = file)
    }
  )

})
