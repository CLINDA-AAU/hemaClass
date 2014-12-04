#install.packages("devtools")
#devtools::install_github("shiny", "rstudio")
#devtools::install_github("shiny-incubator", "rstudio")
library(shiny)
library(tools)
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
  user.affy <- NULL
  results <- list()
  
  GOforReference <- FALSE
  
  #   observe({
  #     df <- hot.to.df(input$hotableClinical)
  #     print(df)
  #   })
  
  
  
  
  
  
  
  
  
  
  # hotable
  
  
  
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
        data
      }
    }else{
      data <- matrix(NaN, ncol =  6, nrow = length(fileInfo$name), 
                     dimnames = list(fileInfo$name, c("Patient", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage")))
      as.data.frame(data)
    }
  }, readOnly = FALSE)
  
  
  
  output$hotableIPI <- renderHotable({ 
    
    input$IPIcalc
    df <- hot.to.df(input$hotableClinical)
    fileInfo <- (input$usrFiles)
    if(!is.null(fileInfo$name)){
      if(input$IPIcalc){
        old.data <- hot.to.df(input$hotableIPI)
        
        data <- matrix(NaN, ncol =  2, nrow = length(fileInfo$name), 
                       dimnames = list(fileInfo$name, c("Patient","IPI")))
        
        data <- as.data.frame(data)
        data$Patient <- fileInfo$name      
        
        if(! is.null(hot.to.df(input$hotableClinical))){
          IPI <- IPIreactive()
          
          data[names(IPI), "IPI"] <- IPI
        }
        
        as.data.frame(data)
        
      }else{    
        data <- matrix(NaN, ncol =  2, nrow = length(fileInfo$name), 
                       dimnames = list(fileInfo$name, c("Patient","IPI")))
        data <- as.data.frame(data)
        data$Patient <- fileInfo$name
        as.data.frame(data)
      }
    }else{
      data <- matrix(NaN, ncol =  2, nrow = length(fileInfo$name), 
                     dimnames = list(fileInfo$name, c("Patient","IPI")))
      as.data.frame(data)
    }
  }, readOnly = FALSE)
  
  
  
  
  IPIreactive <- reactive({   
    fileInfo <- input$usrFiles
    dataC <- hot.to.df(input$hotableClinical)
    
    a <- ifelse(dataC$Age            >   input$AGE.cut, 1, 0)
    b <- ifelse(dataC$ECOG           >   input$ECOG.cut, 1, 0)
    c <- ifelse(dataC$N.Extra.Nodal  >   input$N.Extra.Nodal.cut, 1, 0)
    d <- ifelse(dataC$Stage          >   input$Stage.cut, 1, 0)
    e <- ifelse(dataC$LDH            >   input$LDH.cut, 1, 0)
    
    IPI <- rowSums(data.frame(a, b, c, d, e)) 
    names(IPI) <- dataC$Patient
    IPI
  })
  
  observe({    
    df <- hot.to.df(input$hotableIPI)
    #print(df)
  })
  
  
  
  createReferenceAffy <- reactive({
    # Use isolate() to avoid dependency on input$refFiles
    input$buildreferenceButton
    isolate({
      if(!is.null(input$refFiles)){
        fileInfo <- input$refFiles
        if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))){
          
          showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Not all chosen files are .CEL files.")),
                         styleclass = "error")
          stop("Not all chosen files are .CEL files.")
        }
        hideshinyalert(session, "shinyalertSelectReference")
        user.reference.affy <<- readCelfiles(input$refFiles$datapath)
      }
    })
  })
  
  createReference <- reactive({
    # Take a dependency on createReferenceAffy by calling it
    #input$buildreferenceButton
    
    
    createReferenceAffy()
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      
      
      if(!is.null(user.reference.affy)){
        
        
        user.reference <<- rmaPreprocessing(user.reference.affy)
        user.reference$files <<- input$refFiles$name
        user.reference$exprs <<- NULL
        user.reference$exprs.sc <<- NULL
        
        showshinyalert(session, "shinyalertSelectReferenceSucess",  HTML(paste("The reference was successfully build!")),
                       styleclass = "success")
        showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Press 'normalize files' to start the RMA normalization.", sep="<br/>")),
                       styleclass = "info")
      }
    })
  })
  
  
  createUserAffy <- reactive({
    # Use isolate() to avoid dependency on input$usrFiles
    
    input$normalizeButton
    isolate({
      fileInfo <- input$usrFiles
      if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))){
        stop("Not all chosen files are .CEL files.")
      }
      
      
      if(!is.null(input$usrFiles)){
        hideshinyalert(session, "shinyalertSelectReference") 
        user.affy <<- readCelfiles(input$usrFiles$datapath)
      }
    })
  })
  
  RefUpload <- reactive({
    input$refUpload
    if(is.null(input$refUpload$name))
      return(NULL)
    if(file_ext(input$refUpload$name) != "rds")
      return(NULL)
    ref <<- readRDS(input$refUpload$datapath)
  })
  
  createData <- reactive({
    # Take a dependency on input$normalizeButton by calling it
    #  input$normalizeButton
    hideshinyalert(session, "shinyalertSelectReference") 
    createUserAffy()
    
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      fileInfo <- input$usrFiles
      
      if(!is.null(user.affy)){
        if(input$ChooseMethod == "standardReference")       
          ref <- switch(EXPR = input$ChooseReference,
                        "LLMPPCHOP"  = readLLMPPCHOPreference(),
                        "LLMPPRCHOP" = readLLMPPRCHOPreference(),
                        "MDFCI"      = readMDFCIreference(),
                        "IDRC"       = readIDRCreference())
        
        if(input$ChooseMethod == "build") 
          ref <- user.reference
        
        if(input$ChooseMethod == "upload") 
          RefUpload()
        
        if(input$ChooseMethod != "RMA"){ 
          normalized.data.RMA <<- rmaReference(user.affy, reference = ref)
        }else{
          normalized.data.RMA <<- rmaPreprocessing(user.affy)
        }
        colnames(normalized.data.RMA$exprs) <<- gsub("\\.CEL$", "", input$usrFiles$name)
        colnames(normalized.data.RMA$exprs.sc) <<- gsub("\\.CEL$", "", input$usrFiles$name)
        normalized.data <<- normalized.data.RMA$exprs.sc
        attr(normalized.data, "files") <<- input$usrFiles$name
        
        hideshinyalert(session, "shinyalertResults")
        showshinyalert(session, "shinyalertNormalizationSuccess",  HTML(paste("The normalization was successful!")),
                       styleclass = "success")
      }
    })
  })
  
  
  
  # Function to that calls the classification procedures
  classify <- reactive({
    
    createData()
    
    results <<- list()
    
    # Create or update data if necessary
    
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
    #input$buildreferenceButton
    
    createReferenceAffy()
    createReference()
  })
  
  
  output$start2 <- renderPrint({ 
    #input$normalizeButton    
    # Normalize the data
    createUserAffy()
    createData() # Create or update data if necessary
  })
  
  
  #   
  #   observe({
  #     if (is.null(input$usrFiles)) {
  #       showshinyalert(session, "shinyalertUploadCel",  HTML(paste("No .CEL files chosen!", 
  #                                                                  "Please press 'Choose files' and select the CEL files you want to classify.", sep="<br/>")), 
  #                      styleclass = "info")
  #       showshinyalert(session, "shinyalertInputMeta", paste("Warning: You need to upload CEL files first"), 
  #                      styleclass = "warning")
  #       showshinyalert(session, "shinyalertUploadMeta", paste("Warning: You need to upload CEL files first"), 
  #                      styleclass = "warning")
  #       showshinyalert(session, "shinyalertResults", paste('Warning: You need to upload CEL files first. You can do this at "Load data".'), 
  #                      styleclass = "warning")
  #     }else{
  #       fileInfo <- input$usrFiles
  #       if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))){
  #         
  #         showshinyalert(session, "shinyalertUploadCel",  HTML(paste("Not all chosen files are .CEL files.",
  #                                                                    "Please upload your CEL files again.", sep="<br/>")),
  #                        styleclass = "error")
  #       }else{
  #         hideshinyalert(session, "shinyalertUploadCel")
  #         showshinyalert(session, "shinyalertUploadCelSucces", paste("You have succesfully uploaded", 
  #                                                                    length(fileInfo$name), 
  #                                                                    ifelse(length(fileInfo$name) == 1, "CEL file", "CEL files")),
  #                        styleclass = "success")
  #         
  #         
  #       }
  #       
  #       hideshinyalert(session, "shinyalertInputMeta")
  #       hideshinyalert(session, "shinyalertUploadMeta")
  #       hideshinyalert(session, "shinyalertResults")
  #     }
  #   })
  
  
  output$showNormMethods <- reactive({
    input$usrFiles
    if (is.null(input$usrFiles)) {
      showshinyalert(session, "shinyalertUploadCel",  HTML(paste("No .CEL files chosen!", 
                                                                 "Please press 'Choose files' and select the CEL files you want to classify.", sep="<br/>")), 
                     styleclass = "info")
      showshinyalert(session, "shinyalertInputMeta", paste("Warning: You need to upload CEL files first"), 
                     styleclass = "warning")
      showshinyalert(session, "shinyalertUploadMeta", paste("Warning: You need to upload CEL files first"), 
                     styleclass = "warning")
      showshinyalert(session, "shinyalertResults", paste('Warning: You need to upload CEL files first. You can do this at "Load data".'), 
                     styleclass = "warning")
      
      return(0)
    }else{
      fileInfo <- input$usrFiles
      if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))){
        
        showshinyalert(session, "shinyalertUploadCel",  HTML(paste("Not all chosen files are .CEL files.",
                                                                   "Please upload your CEL files again.", sep="<br/>")),
                       styleclass = "error")
        return(0)
      }else{
        hideshinyalert(session, "shinyalertUploadCel")
        showshinyalert(session, "shinyalertUploadCelSucces", paste("You have succesfully uploaded", 
                                                                   length(fileInfo$name), 
                                                                   ifelse(length(fileInfo$name) == 1, "CEL file", "CEL files")),
                       styleclass = "success")   
        
        showshinyalert(session, "shinyalertResults", paste('Warning: You need to pre-process the CEL files first. You can do this at "Load data".'), 
                       styleclass = "warning")
        
        hideshinyalert(session, "shinyalertInputMeta")
        hideshinyalert(session, "shinyalertUploadMeta")
        return(1)
      }
    }
  })
  
  output$showNormButton <- reactive({
    createReference()
    RefUpload()  
    input$usrFiles
    ShowDownloadReference <<- 0
    if (!is.null(input$usrFiles)) {
      fileInfo <- input$usrFiles
      if (all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))){
        if(input$ChooseMethod == "blah"){
          showshinyalert(session, "shinyalertSelectReference",  HTML("Please select a reference for RMA-preprocessing"), 
                         styleclass = "info")
          return(0) # don't show
        }else{
          if(input$ChooseMethod == "build"){
            
            if (is.null(input$refFiles)){
              showshinyalert(session, "shinyalertSelectReference",
                             HTML(paste("No .CEL files chosen for building the reference!",
                                        "Please press 'Choose files' and select the CEL files you want to use for building the classifier", 
                                        sep = "<br/>" )),
                             styleclass = "info")
              return(0)
            }else{
              if (!all(grepl("\\.CEL$", input$refFiles$name, ignore.case = TRUE))){
                
                showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Not all chosen files are .CEL files.",
                                                                                 "Please upload your reference CEL files again.", sep="<br/>")),
                               styleclass = "error")
                return(0)
              }else{
                if(length(input$refFiles$name) == 1){
                  showshinyalert(session, "shinyalertSelectReference",  HTML(paste("You only selected 1 .CEL file for building the reference.",
                                                                                   "Please upload more than 1 CEL file before you continue.", sep="<br/>")),
                                 styleclass = "error")
                 
                  return(0)
                }else{                
                  if(is.null(user.reference) ||  any(input$refFiles$name != user.reference$files )){
                    showshinyalert(session, "shinyalertSelectReference",  HTML("Press 'Build the reference' to start the RMA reference building."),
                                   styleclass = "info")
                   
                    return(0)
                  }else{
                    if(is.null(normalized.data) || any(fileInfo$name != attr(normalized.data, "files"))){
                      showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Press 'normalize files' to start the RMA normalization.",
                                                                                       "You can download the established reference by clicking 'Download reference for later use'", sep="<br/>")),
                                     styleclass = "info")  
                      ShowDownloadReference <<- 1 
                      return(1)
                    }                  
                  }
                }            
              }
            }
          }
          if(input$ChooseMethod == "upload"){
            if(is.null(input$refUpload)){
              showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Please upload a reference.", sep="<br/>")),
                             styleclass = "info")  
              return(0)
            }else{
              if(file_ext(input$refUpload$name) != "rds"){
                showshinyalert(session, "shinyalertSelectReference",  HTML(paste("The uploaded file is not an 'rds' file.", "Please upload a correct reference", sep="<br/>")),
                               styleclass = "error")
                return(0)
              }
              RefUpload()
              if(!is.null(attributes(ref)) && "Version" %in% names(attributes(ref))  && attr(ref, "Version") %in% "hemaClass"){
                showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Press 'normalize files' to start the RMA normalization.", sep="<br/>")),
                               styleclass = "info")  
                return(1)            
              }else{   
                showshinyalert(session, "shinyalertSelectReference",  HTML(paste("The uploaded file cannot be used as a reference.", "Please upload a correct reference", sep="<br/>")),
                               styleclass = "error")
                return(0)
              }
            }
          }
          if(input$ChooseMethod == "standardReference"){
            showshinyalert(session, "shinyalertSelectReference",  
                           HTML(paste("Select the cohort you want to use as a reference",
                                      "",
                                      "The possible references are:",
                                      "&#160 LLMPP CHOP:",
                                      "&#160 LLMPP R-CHOP:",
                                      "&#160 IDRC:",
                                      "&#160 MDFCI:",
                                      "",
                                      "When chosen:",
                                      "Press 'normalize files' to start the RMA normalization.",
                                      sep="<br/>")),
                           styleclass = "info")  
            return(1)
          }
          
          if(input$ChooseMethod == "RMA"){
            showshinyalert(session, "shinyalertSelectReference",  
                           HTML(paste("Press Normalize files to pre-process the uploaded CEL files according to the RMA method",
                                      sep="<br/>")),
                           styleclass = "info")  
            return(1)
          }
          
          1
        }
      }
    }else{
      return(0)
    }
  })
  
  
  #   observe({
  #     input$usrFiles
  #     input$refFiles
  #     fileInfo <- input$usrFiles
  #     if (!is.null(input$usrFiles)) {
  #       fileInfo <- input$usrFiles
  #       if (all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))){
  #         if(input$ChooseMethod == "blah"){
  #           showshinyalert(session, "shinyalertSelectReference",  HTML("Please select a reference for RMA-preprocessing"), 
  #                          styleclass = "info")
  #         }
  #         
  #         if (input$ChooseMethod == "build"){
  #           if (is.null(input$refFiles)){
  #             showshinyalert(session, "shinyalertSelectReference",
  #                            HTML(paste("No .CEL files chosen for building the reference!",
  #                                       "Please press 'Choose files' and select the CEL files you want to use for building the classifier", 
  #                                       sep = "<br/>" )),
  #                            styleclass = "info") 
  #           }else{
  #             if (!all(grepl("\\.CEL$", input$refFiles$name, ignore.case = TRUE))){
  #               
  #               showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Not all chosen files are .CEL files.",
  #                                                                                "Please upload your reference CEL files again.", sep="<br/>")),
  #                              styleclass = "error")
  #               hideshinyalert(session, "shinyalertNormalizationSuccess")
  #             }else{
  #               if(length(input$refFiles$name) == 1){
  #                 showshinyalert(session, "shinyalertSelectReference",  HTML(paste("You only selected 1 .CEL file for building the reference.",
  #                                                                                  "Please upload more than 1 CEL file before you continue.", sep="<br/>")),
  #                                styleclass = "error")
  #               }else{                
  #                 if(is.null(user.reference) ||  any(input$refFiles$name != user.reference$files )){
  #                   showshinyalert(session, "shinyalertSelectReference",  HTML("Press 'Build the reference' to start the RMA reference building."),
  #                                  styleclass = "info")
  #                   hideshinyalert(session, "shinyalertNormalizationSuccess")
  #                   
  #                 }else{
  #                   if(any(fileInfo$name != attr(normalized.data, "files"))){
  #                     showshinyalert(session, "shinyalertSelectReference",  HTML(paste("Press 'normalize files' to start the RMA normalization.", sep="<br/>")),
  #                                    styleclass = "info")  
  #                     hideshinyalert(session, "shinyalertNormalizationSuccess")
  #                   }                  
  #                 }
  #               }            
  #             }
  #           }
  #         }
  #       }
  #     }  
  #   })
  
  
  output$showDownloadRefButton <- reactive({  
    createReference()
    ifelse(!is.null(user.reference), 1, 0)
  })
  
  
  output$showBuildRefButton <- reactive({
    
    input$refFiles
    
    if(input$ChooseMethod != "build")
      return(0) # don't show
    if(is.null(input$refFiles) || length(input$refFiles$name) < 2)
      return(0)
    
    return(1)
    
  })
  
  output$showErrorprints <- reactive({
    0 # don't show
    # 1# Show  
  })
  
  outputOptions(output, "showNormMethods", suspendWhenHidden = FALSE)
  outputOptions(output, "showNormButton", suspendWhenHidden = FALSE)
  outputOptions(output, "showDownloadRefButton", suspendWhenHidden = FALSE)
  outputOptions(output, "showBuildRefButton", suspendWhenHidden = FALSE)
  outputOptions(output, "showErrorprints", suspendWhenHidden = FALSE)
  outputOptions(output, "start", suspendWhenHidden = FALSE)
  outputOptions(output, "start2", suspendWhenHidden = FALSE)
  
  
  
  # output$start <- renderPrint({    
  #   # Take dependency on input$normalizeButton by calling it
  #   input$normalizeButton
  #   input$buildreferenceButton
  #   
  #   if (is.null(input$usrFiles))
  #     return(cat("No .CEL files chosen!",
  #                "\nPlease press 'Choose files' and select the CEL files you want to classify"))
  #   
  #   if(input$ChooseMethod == "blah")
  #     return(cat("Please select a reference for RMA-preprocessing"))
  #   
  #   if (is.null(input$refFiles) & input$ChooseMethod == "build")
  #     return(cat("No .CEL files chosen for building the reference!",
  #                "\nPlease press 'Choose files' and select the CEL files", 
  #                "\nyou want to use for building the classifier"))
  #   
  #   if (is.null(user.reference) & input$buildreferenceButton == 0 & input$ChooseMethod == "build")
  #     return(cat("Press 'Build the reference' to start the RMA reference building."))
  #   
  #   if (input$ChooseMethod == "build" & is.null(user.reference) & input$normalizeButton == 0){
  #     createReferenceAffy()
  #     createReference()
  #     return(cat("The reference was successfully build!",
  #                "\nPress 'normalize files' to start the RMA normalization."))
  #   }
  #   
  #   if (input$ChooseMethod == "build"){
  #     createReferenceAffy()
  #     createReference()
  #   } 
  #   
  #   if (input$ChooseMethod == "upload" & is.null(input$refUpload))
  #     return(cat("Please upload the reference"))
  #   
  #   
  #   if (is.null(normalized.data) & input$normalizeButton == 0)
  #     return(cat("Press 'normalize files' to start the RMA normalization."))
  #   
  #   # Normalize the data
  #   createData() # Create or update data if necessary
  #   cat("The normalization was successful!")
  # })
  
  
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
  
  output$normalizedData <- renderDataTable({ 
    createData()
    
    if (is.null(normalized.data))
      return(NULL)
    
    data.frame(Probeset = row.names(normalized.data), normalized.data)[1:100, , drop = FALSE]
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
    createData() # Create or update data if necessary
    if (is.null(normalized.data.RMA))
      return(NULL)
    
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
      attr(user.reference, "Version") <- "hemaClass"
      saveRDS(object = user.reference, file = file)
    }
  )
  
})
