# Load packages
library("shiny")
library("tools")
library("hemaClass")
library("shinysky")
library("gdata")
library("survival")
library("WriteXLS")
cat("Packages loaded\n")

# Changing maximum file size
options(shiny.maxRequestSize = 30*1024^2)

# Used icons in messages etc.
ERROR   <- paste(icon("remove-sign", lib = "glyphicon"), 
                 tags$strong("Error:"))
WARNING <- paste(icon("exclamation-sign", lib = "glyphicon"), 
                 tags$strong("Warning:"))
SUCCES  <- paste(icon("ok-sign", lib = "glyphicon"), 
                 tags$strong("Succes:"))
TO <- icon("arrow-right", lib = "glyphicon") # Right arrow
HANDLEFT <- icon("hand-left", lib = "glyphicon")

# Common error messages
non.cel.files.uploaded.text <- HTML(
  paste(ERROR, "Not all chosen files are .CEL files.<br/> 
                Please try again and upload only .CEL files.")
)


# Convert probabilites to text
probToText <- function(p, symmetric = FALSE, append.p = TRUE) {
  stopifnot(all(p <= 1))
  stopifnot(all(p >= 0))
  if (symmetric) {
    pp <- abs(p - .5) + .5
  } else {
    pp <- p
  }
  brks <- c(0, 0.01, 0.05, 0.1, 0.3, 0.7, 0.9, 0.95, 0.99, 1)
  lbls <- paste(c("extremely low", 
                  "very low",
                  "low",
                  "moderately low",
                  "intermediate",
                  "moderately high",
                  "high",
                  "very high",
                  "extremely high"),
                "probability")
  txt <- cut(pp, breaks = brks, include.lowest = TRUE, labels = lbls)
  txt <- paste0(txt, sprintf(ifelse(append.p, " (p = %0.2f)", ""), p))
  txt <- gsub("p = 0.00", "p < 0.01", txt)
  txt <- gsub("p = 1.00", "p > 0.99", txt)
  return(txt)
}

# Default colors
def.cols <- c("#9199D1", "#D2786B", "#CED948", "#80DEBC", "#79D472", "#D4C990",
              "#D78E3D", "#D180AE", "#C4B2B5", "#6CB4C6", "#9F9A3D", "#72966F",
              rainbow(255))

# Debug
verbose <- TRUE


#
# Run the shiny server
#

shinyServer(function(input, output, session) {
  if (verbose) cat("Shinyserver started.\n")
  showModal(modalDialog("DISCLAIMER: The predictive model and all information available on and presented
                                   on this webpage was developed for and is intended for research purposes
                                   only. The predictive model and all information available on this webpage
                                   must under no circumstances be used, wholly or partly, for any clinical
                                   decision making and in any other way than for research purposes.
                                   The predictive model and all information available on and presented on this
                                   webpage is intended for use for health care professionals (researchers) only
                                   and should under no circumstances be used by any non-health care professional.
                                   If you are a patient or a relative, we strongly advice against using the
                                   predictive model and all information available on and presented on this webpage.
                                   Instead you should consult your attending physician for prognostic information.
                                   We, the authors of the hemaClass package and it's related papers,
                                   are not liable in any form or way for any damages, direct or indirect,
                                   arising out of or in connection to the use of the predictive model or any
                                   information available on and presented on this webpage.",
    easyClose = TRUE
  ))
  
  addResourcePath('data', system.file("extdata/celfiles", package = "hemaClass"))
  
  # Initialzing the local objects to be filled
  normalized.data <- NULL
  normalized.data.rle.stats <- NULL
  rle.stats <- NULL
  normalized.data.mean <- NULL
  normalized.data.RMA <- NULL
  user.reference  <- NULL
  user.reference.affy <- NULL
  user.affy <- NULL
  results <- list()
  
  LoadAnnotation <- reactive({
    if (verbose) cat("LoadAnnotation called\n")
    HGU133Plus2 <<- 
      readRDS("Database/Annotation/HGU133Plus2.na33.annot.rds")
  })
  
  buildin.data <- list()
  buildin.datasets <- 
    setdiff(dir("Database/"), c("Annotation", "Classification"))
  
  chosenDataset <- NULL
  
     
  
  ##############################################################################
  ##
  ## Build in data
  ##
  ##############################################################################
  
  
  output$buildindataselector <- renderUI({
    data.list <- list()
    for (data.iter in c("Choose", buildin.datasets)) {
      data.list[[data.iter]] <- data.iter
    }
    selectInput(inputId = "choosebuildidDataset", 
                label   = "Choose a dataset",
                choices = data.list)
  })
  
  
  loadbuildinData <- reactive({     
    if (verbose) cat("loadbuildinData called.\n")
    
    if (is.null(buildin.data)) {
      buildin.data <<- list()
    }
    dataset <- input$choosebuildidDataset
    
    if (is.null(dataset) || dataset == "Choose") {
      return(buildin.data)
    }
    
    
    if (dataset %in% buildin.datasets) {
      if (!(dataset %in% names(buildin.data))) {
        hideshinyalert(session, "shinyalertResults")
        
        # Organize GEP
        GEP.file <- dir(file.path("Database/", dataset, "GEP"), 
                        full.names = TRUE, pattern = ".rds")
        GEP.data.temp <- readRDS(GEP.file)
        GEP <- microarrayScale(exprs(GEP.data.temp))
        colnames(GEP) <- gsub("\\.CEL(.gz)?$", "", colnames(GEP),
                              ignore.case = TRUE)
        buildin.data[[dataset]][["GEP"]] <<- GEP
        
        GEP.mean <- microarrayScale(exprs(GEP.data.temp), center = "mean")
        colnames(GEP) <- gsub("\\.CEL(.gz)?$", "", colnames(GEP.mean), 
                              ignore.case = TRUE)
        buildin.data[[dataset]][["GEP.mean"]] <<- GEP.mean
        
        # Organize metadata
        Meta.file <- dir(file.path("Database/", dataset, "Metadata"), 
                         full.names = TRUE, pattern = ".rds")
        meta <- readRDS(Meta.file)  
        if (!is.null(meta$GEO.ID)) rownames(meta) <- meta$GEO.ID
        rownames(meta) <- gsub("\\.CEL(.gz)?$", "", rownames(meta),
                               ignore.case = TRUE)
        buildin.data[[dataset]][["metadata"]] <<- meta
      }
    }      
    return(buildin.data)
  })
  
  
  output$buildinMetaData <- renderDataTable({ 
    loadbuildinData()
    dataset <- input$choosebuildidDataset
    
    if (is.null(dataset) || dataset == "Choose") {
      return(NULL)
    }
    
    if (dataset %in% buildin.datasets) {
      if (dataset %in% names(buildin.data)) {
        as.data.frame(buildin.data[[dataset]][["metadata"]])
      }
    }
  })
  
  output$buildinGEP <- renderDataTable({ 
    # input$buildindataselector
    loadbuildinData()
    
    dataset <- input$choosebuildidDataset
    if (is.null(dataset) || dataset == "Choose") {
      return(NULL)
    }
    
    if (dataset %in% buildin.datasets) {
      if (dataset %in% names(buildin.data)) {
        as.data.frame(buildin.data[[dataset]][["GEP"]]) 
      }
    }
  })
  
  
  buildInMetadata <- reactive({
    if (verbose) cat("buildInMetadata called.\n")
    loadbuildinData()
    dataset <- input$choosebuildidDataset
    
    if (is.null(dataset) || dataset == "Choose") {
      return(NULL)
    }
    
    if (dataset %in% buildin.datasets) {
      if (dataset %in% names(buildin.data)) {   
        data <- as.data.frame(buildin.data[[dataset]][["metadata"]])
        chosenDataset <<- "Build-in dataset"
        return(data)   
      } 
    }
  })
  
   
  
  GEPInUse <- reactive({ 
    if (verbose) cat("GEPInUse called.\n")
    
    #input$chooseMetaDataset
    #MetaDataInUse()
    GEP.in.use <- NULL    
    
    #if (!is.null(input$chooseMetaDataset)) {
      #if (input$chooseMetaDataset == "Build-in dataset") {
	  if( length(buildin.data) > 0){
        dataset <- input$choosebuildidDataset
        GEP.in.use <- as.data.frame(buildin.data[[dataset]][["GEP"]])
        return(GEP.in.use)
      }
    #}
    
    createData() 
	includeSamples=normalized.data.rle.stats[,3] < input$rle.iqr.threshold
	
    if (!is.null(normalized.data) & sum(includeSamples)>0) {
	  GEP.in.use <- normalized.data[,includeSamples, drop=FALSE]
      return(GEP.in.use)
    }   
    return(GEP.in.use)
  })
  
  GEPInUse2 <- reactive({ 
    if (verbose) cat("GEPInUse2 called.\n")
    
    #MetaDataInUse()
    GEP.in.use <- NULL    
    
    #if (!is.null(input$chooseMetaDataset)) {
      #if (input$chooseMetaDataset == "Build-in dataset") {
	  if( length(buildin.data) > 0){
        dataset <- input$choosebuildidDataset
        GEP.in.use <- as.data.frame(buildin.data[[dataset]][["GEP.mean"]])
        return(GEP.in.use)
      }
    #}
    
    createData() 
	includeSamples=normalized.data.rle.stats[,3] < input$rle.iqr.threshold
	
    if (!is.null(normalized.data.mean) & sum(includeSamples)>0) {
	  GEP.in.use <- normalized.data.mean[,includeSamples, drop=FALSE]
      return(GEP.in.use)
    }   
    
    return(GEP.in.use)
  })
  
  
  ##############################################################################
  ##
  ## Upload .CEL files
  ##
  ##############################################################################
  
  createReferenceAffy <- reactive({
    if (verbose) cat("createReferenceAffy called.\n")
    
    input$buildreferenceButton
    isolate({
      if (!is.null(input$refFiles)) {
        fileInfo <- input$refFiles
        if (!all(grepl("\\.CEL(.gz)?$", fileInfo$name, ignore.case = TRUE))) {
          
          showshinyalert(session, "shinyalertSelectReference",  
                         non.cel.files.uploaded.text,
                         styleclass = "danger")
        }
        
        hideshinyalert(session, "shinyalertSelectReference")
        user.reference.affy <<- readCelfiles(input$refFiles$datapath)
      }
    })
  })
  
  createReference <- reactive({
    if (verbose) cat("createReference called.\n")
    # Take a dependency on createReferenceAffy by calling it
    #input$buildreferenceButton

    createReferenceAffy()
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      
      if (!is.null(user.reference.affy)) {
      
        user.reference <<- rmaPreprocessing(user.reference.affy)
        user.reference$files <<- input$refFiles$name
        user.reference$exprs <<- NULL
        user.reference$exprs.sc <<- NULL
        
        showshinyalert(session, "shinyalertSelectReferenceSucess",  
                       HTML(paste(SUCCES, 
                                  "The reference was successfully build!")),
                       styleclass = "success")
        showshinyalert(session, "shinyalertSelectReference",  
                       HTML("Press 'normalize files' to start the RMA normalization."),
                       styleclass = "info")
      }
    })
  })
  
  
  createUserAffy <- reactive({
    if (verbose) cat("createUserAffy called.\n")
    
    input$normalizeButton
    isolate({
      fileInfo <- input$usrFiles
      if (!all(grepl("\\.CEL(.gz)?$", fileInfo$name, ignore.case = TRUE))) {
        stop("Not all chosen files are .CEL files.")
      }
      
      if (!is.null(input$usrFiles)) {
        hideshinyalert(session, "shinyalertSelectReference") 
        user.affy <<- readCelfiles(input$usrFiles$datapath)
      }
    })
  })
  
  RefUpload <- reactive({
    if (verbose) cat("RefUpload called.\n")
    
    input$refUpload
    if (is.null(input$refUpload$name)) {
      return(NULL)
    }
    if (file_ext(input$refUpload$name) != "rds") {
      return(NULL)
    }
    ref <<- readRDS(input$refUpload$datapath)
  })
  
  createData <- reactive({
    if (verbose) cat("createData called.\n")
	# Reset Build in data if previously used
	buildin.data <<- NULL
	    
    # Take a dependency on input$normalizeButton by calling it
    #  input$normalizeButton
    hideshinyalert(session, "shinyalertSelectReference") 
    createUserAffy()
    
    # Use isolate() to avoid dependency on input$usrFiles
    isolate({
      fileInfo <- input$usrFiles
		
      if (!is.null(user.affy)) {
        # LoadAnnotation()
        if (input$ChooseMethod == "standardReference") {       
          ref <- switch(EXPR = input$ChooseReference,
                        "LLMPPCHOP"  = readLLMPPCHOPreference(),
                        "LLMPPRCHOP" = readLLMPPRCHOPreference(),
                        "MDFCI"      = readMDFCIreference(),
                        "IDRC"       = readIDRCreference(),
                        "CHEPRETRO"  = readCHEPRETROreference(),
                        "UAMS"       = readUAMSreference())
        }
        if (input$ChooseMethod == "build") {
          ref <- user.reference
        }
        if (input$ChooseMethod == "upload") {
          RefUpload()
        }
        if (input$ChooseMethod != "RMA") { 
          normalized.data.RMA <<- rmaReference(user.affy, reference = ref)
        } else {
          normalized.data.RMA <<- rmaPreprocessing(user.affy)
        }
        
        colnames(normalized.data.RMA$exprs) <<- 
          gsub("\\.CEL(.gz)?$", "", input$usrFiles$name, ignore.case = TRUE)
        colnames(normalized.data.RMA$exprs.sc) <<-
          gsub("\\.CEL(.gz)?$", "", input$usrFiles$name, ignore.case = TRUE)
		rownames(normalized.data.RMA$RLE.stats) <<-
          gsub("\\.CEL(.gz)?$", "", input$usrFiles$name, ignore.case = TRUE)
        
        normalized.data <<- normalized.data.RMA$exprs.sc
        attr(normalized.data, "files") <<- input$usrFiles$name
        
        normalized.data.mean <<- normalized.data.RMA$exprs.sc.mean
        attr(normalized.data.mean, "files") <<- input$usrFiles$name
		
		normalized.data.rle.stats <<- normalized.data.RMA$RLE.stats
        attr(normalized.data.rle.stats, "files") <<- input$usrFiles$name
		       			
        hideshinyalert(session, "shinyalertResults")
        showshinyalert(session, "shinyalertNormalizationSuccess",  
                       HTML(paste(SUCCES, "The normalization was successful!")),
                       styleclass = "success")
      }
    })
  })
  
		  
  ##############################################################################
  ##
  ## The classification
  ##
  ##############################################################################
  
  observeEvent(input$resetButton, {
    if ("ABCGCB" %in% input$getClassifications) {
      updateSliderInput(session, "nc.range", value = c(0.1, 0.9))
    }
    if ("Rituximab (R)" %in% input$getClassifications) {
      updateSliderInput(session, "Rituximab.range", value = c(0.38, 0.54))
    }
    if ("Cyclophosphamide (C)" %in% input$getClassifications) {
      updateSliderInput(session, "Cyclophosphamide.range", 
                        value = c(0.46,0.67))
    }
    if ("Doxorubicin (H)" %in% input$getClassifications) {
      updateSliderInput(session, "Doxorubicin.range", value = c(0.1, 0.86))
    }
    if ("Vincristine (O)" %in% input$getClassifications) {
      updateSliderInput(session, "Vincristine.range", value = c(0.38, 0.54))
    }
    if ("Combined (CHO)" %in% input$getClassifications) {
      updateSliderInput(session, "Combined.range", value = c(0.07, 0.91))
    }
  })
  
  MelphalanClassifierR <- reactive({
    if (verbose) cat("MelphalanClassifierR called.\n")
    
    normalized.data <- GEPInUse2()
    MelphalanClassifier(normalized.data)
  })
  
  BAGSR <- reactive({
    if (verbose) cat("BAGSR called.\n")
    
    normalized.data <- GEPInUse()
    BAGS(normalized.data, cut.spec = 0)
  })
  
  ABCGCBR <- reactive({
    if (verbose) cat("ABCGCBR called.\n")
    
    normalized.data <- GEPInUse()
    ABCGCB(normalized.data, NC.range = input$nc.range)
  })
  
  DexamethasoneClassifierR <- reactive({
    if (verbose) cat("DexamethasoneClassifierR called.\n")
    
    normalized.data <- GEPInUse()
    probs <- as.numeric(DexamethasoneClassifier(normalized.data))
    class <- cut(probs,
                 c(-Inf, input$Dexamethasone.range, Inf),
                 labels = c("Resistant", "Intermediate", "Sensitive"))
    return(list(probs = probs, class = class))
  })
  
  
  RituximabClassifierR <- reactive({
    if (verbose) cat("RituximabClassifierR called.\n")
    
    normalized.data <- GEPInUse()
    type <- "uncorrected"
    if (input$HSCorrected) {
      type <- "corrected"
    }
    if (input$Lysis) {
      type <- "lysis2"
    }
    
    rit <- RituximabClassifier(normalized.data, type = type, 
                               cut = input$Rituximab.range, cut.spec = 0)
    
    if (input$Lysis) {
      probs <-  apply(rit$prob, 1, max)
      class <-  rit$class[,1]
    } else {
      probs <- rit$prob[,1]
      class <- rit$class[,1]
    }
    
    return(list(probs = probs, class = class))
  })
  
  ResistanceClassifierR <- reactive({
    if (verbose) cat("ResistanceClassifierR called.\n")
    
    normalized.data <- GEPInUse()
    av.drugs <<- c("Cyclophosphamide", "Doxorubicin", "Vincristine") 

    cut <- list(Cyclophosphamide = input$Cyclophosphamide.range,
                Doxorubicin = input$Doxorubicin.range,
                Vincristine = input$Vincristine.range,
                Combined    = input$Combined.range)
    
    ResistanceClassifier(normalized.data, drugs = c(av.drugs, "Combined"), 
                         cut = cut)
  })
  
  classify <- reactive({
    if (verbose) cat("classify called.\n")
    
    normalized.data <- GEPInUse()
    normalized.data.mean <- GEPInUse2()
    
    results <<- list()
    chosen.names <<- vector()
    
    # Create or update data if necessary
    if (is.null(normalized.data) ) {
      return(NULL)
    }
    
    results$files <<- colnames(normalized.data)

    bags <- BAGSR()
    results$ProbOfBAGS <<- bags$prob
    results$BAGS <<- bags$class
    if ("BAGS" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "ProbOfBAGS", "BAGS")
    } else {
      chosen.names <<- setdiff(chosen.names, c("ProbOfBAGS", "BAGS"))
    }
    
    abcgcb <- ABCGCBR()
    results$"ProbOfABC" <<- abcgcb$prob
    results$"ABCGCB" <<- abcgcb$class
    if ("ABCGCB" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "ProbOfABC", "ABCGCB")
    } else {
      chosen.names <<- setdiff(chosen.names, c("ProbOfABC", "ABCGCB"))
    }
    
    results$ABCGCB2 <<- results$ABCGCB
    results$ABCGCB2 <<- as.character(results$ABCGCB2)
    
    results$ABCGCB2[results$ABCGCB2 == "GCB" & results$BAGS == "Centrocyte"] <<- "GCB-CC"
    results$ABCGCB2[results$ABCGCB2 == "GCB" & results$BAGS == "Centroblast"] <<- "GCB-CB"
    if ("ABCGCB2" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "ABCGCB2")
    } else {
      chosen.names <<- setdiff(chosen.names, c("ABCGCB2"))
    }
    
    Rtx <- RituximabClassifierR()
    results$"RtxProb"  <<- Rtx$probs
    results$"RtxClass" <<- Rtx$class
    if ("Rituximab (R)" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "RtxProb", "RtxClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("RtxProb", "RtxClass"))
    }
    
    CHO.single <<- ResistanceClassifierR()
    results$CycProb  <<- CHO.single$prob[,"Cyclophosphamide"]
    results$CycClass <<- CHO.single$class[,"Cyclophosphamide"]
    if ("Cyclophosphamide (C)" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "CycProb", "CycClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("CycProb", "CycClass")) 
    }
    
    results$DoxProb  <<- CHO.single$prob[,"Doxorubicin"]
    results$DoxClass <<- CHO.single$class[,"Doxorubicin"]
    if ("Doxorubicin (H)" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "DoxProb", "DoxClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("DoxProb", "DoxClass"))
    }
    
    results$VinProb  <<- CHO.single$prob[,"Vincristine"]
    results$VinClass <<- CHO.single$class[,"Vincristine"]
    if ("Vincristine (O)" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "VinProb", "VinClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("VinProb", "VinClass")) 
    }
    
   # Dex <- DexamethasoneClassifierR()
   # results$"DexProb"  <<- Dex$probs
   # results$"DexClass" <<- Dex$class
   # if ("Dexamethasone" %in% input$getClassifications) {
   #   chosen.names <<- c(chosen.names, "DexProb", "DexClass")
   # } else {
   #   chosen.names <<- setdiff(chosen.names, c("DexProb", "DexProb"))
   # }
    
    results$CombProb  <<- CHO.single$prob[,"Combined"]
    results$CombClass <<- CHO.single$class[,"Combined"]
    if ("Combined (CHO)" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "CombProb", "CombClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("CombProb", "CombClass"))
    }
    
    Mel <- MelphalanClassifierR()
    results$"MelProb" <<- apply(Mel$probs, 1, max)
    results$"MelClass" <<- Mel$class
    if ("Melphalan" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "MelProb", "MelClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("MelProb", "MelClass"))
    }
    
    results <<- base::as.data.frame(results, row.names = NULL)
    rownames(results) <<- NULL
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
  
  
    
  ##############################################################################
  ##
  ## Alerts/feedback and advancement in pre-processing
  ##
  ##############################################################################
  
  output$showNormMethods <- reactive({
    if (verbose) cat("showNormMethods called.\n")
    input$usrFiles
    if (is.null(input$usrFiles)) {
      no.uploaded.celfiles.text <- HTML(
        paste(WARNING, "You need to upload .CEL files first. You can do this 
              under the <strong>Load data</strong>", TO, "<strong>CEL 
              files</strong> tab.")
      )
      info.text <- HTML(
        paste("No .CEL files chosen yet.", tags$br(),
              HANDLEFT, "Please press <strong>Choose 
              files</strong> and select the .CEL files you want to classify.")
      )
      showshinyalert(session, "shinyalertUploadCel", info.text, 
                     styleclass = "info")
      showshinyalert(session, "shinyalertResults", no.uploaded.celfiles.text, 
                     styleclass = "warning")
      return(0)
    } else {
      fileInfo <- input$usrFiles
      if (!all(grepl("\\.CEL(.gz)?$", fileInfo$name, ignore.case = TRUE))) {
        showshinyalert(session, "shinyalertUploadCel",  
                       non.cel.files.uploaded.text,
                       styleclass = "danger")
        return(0)
      }
      check <-
        sapply(fileInfo$datapath, function(x){  
          affyio::read.celfile.header(as.character(x))$cdfName})
      
      if (!all(check == "HG-U133_Plus_2")) {
        
        nogood <- fileInfo$name[check != "HG-U133_Plus_2"]
        message <- 
          paste("Only the Human Genome U133 Plus 2.0 Array", 
                "is currently supported.",br(),
                ifelse(length(nogood) == 1, 
                       "This .CEL file is currently not supported:",
                       "These .CEL files are currently not supported: <br/> "),
                paste(paste(nogood, check[check != "HG-U133_Plus_2"], 
                            sep = ": "), collapse = "<br/> "))
        showshinyalert(session, "shinyalertUploadCel",  
                       HTML(message),
                       styleclass = "danger")
        return(0)
      } else {
        upload.success.text <- HTML(
          paste(icon("ok-sign", lib = "glyphicon"),
                "Succesfully uploaded", length(fileInfo$name), 
                ifelse(length(fileInfo$name) == 1, ".CEL file.", ".CEL files."))
        )
        missing.preprocess.text <- HTML(
          paste(WARNING, "You need to pre-process the .CEL files first. You can 
                do this under <strong>Load data</strong>", TO, 
                "<strong>CEL files</strong>.")
        )
        hideshinyalert(session, "shinyalertUploadCel")
        showshinyalert(session, "shinyalertUploadCelSucces",
                       upload.success.text,
                       styleclass = "success")   
        showshinyalert(session, "shinyalertResults",
                       missing.preprocess.text, 
                       styleclass = "warning")             
        return(1)
      }
    }
  })
  
  output$showNormButton <- reactive({
    if (verbose) cat("showNormButton called.\n")
    
    createReference()
    RefUpload()  
    input$usrFiles
    if (!is.null(input$usrFiles)) {
      fileInfo <- input$usrFiles
      if (all(grepl("\\.CEL(.gz)?$", fileInfo$name, ignore.case = TRUE))) {
        if (input$ChooseMethod == "blah") {
          info.text <- HTML(
            paste(HANDLEFT, "Please select the RMA pre-processing method.")
          )
          showshinyalert(session, "shinyalertSelectReference",
                         info.text, 
                         styleclass = "info")
          return(0) # don't show
        } else {
          if (input$ChooseMethod == "build") {
            if (is.null(input$refFiles)) {
              info.text <- HTML(
                paste("No .CEL files chosen for building the reference!<br/>",
                      HANDLEFT, "Please press <strong>Choose files</strong> 
                      and select the .CEL files you want to use for  building 
                      the classifier.")
              )
              showshinyalert(session, "shinyalertSelectReference", info.text,
                             styleclass = "info")
              return(0)
            } else {
              if (!all(grepl("\\.CEL(.gz)?$", input$refFiles$name, 
                             ignore.case = TRUE))) {
                showshinyalert(session, "shinyalertSelectReference",  
                               non.cel.files.uploaded.text,
                               styleclass = "danger")
                return(0)
              } else {
                
                check <-
                  sapply(input$refFiles$datapath, function(x){ 
                    affyio::read.celfile.header(as.character(x))$cdfName})
                
                if (!all(check == "HG-U133_Plus_2")) {
                  
                  nogood <- input$refFiles$name[check != "HG-U133_Plus_2"]
                  message <- 
                    paste("Only the Human Genome U133 Plus 2.0 Array", 
                          "is currently supported.", br(),
                          ifelse(length(nogood) == 1, 
                                 "This .CEL file is currently not supported:",
                                 "These .CEL files are currently not supported:
                                 <br/> "),
                          paste(paste(nogood, check[check != "HG-U133_Plus_2"],
                                      sep = ": "), collapse = "<br/> "))
                  showshinyalert(session, "shinyalertSelectReference",
                                 HTML(message),
                                 styleclass = "danger")
                  return(0)
                
              } else {
                if (length(input$refFiles$name) == 1) {
                  showshinyalert(session, "shinyalertSelectReference",  
                                 HTML("You only selected 1 .CEL file for 
                                      building the reference.<br/> Please 
                                      upload more than 1 CEL file before you 
                                      continue."),
                                 styleclass = "danger")
                  return(0)
                } else {
                  if (length(input$refFiles$name) < 30) {
                    few.uploaded.cel.files.text <- HTML(
                      paste(WARNING, "You selected less than 30 .CEL files for  
                            building the reference.<br/> The performance is not 
                            documented for less than 30 .CEL files.")
                    )
                    showshinyalert(session, "shinyalertSelectReference", 
                                   few.uploaded.cel.files.text,
                                   styleclass = "warning")
                  } else {
                    if (is.null(user.reference) || 
                        any(input$refFiles$name != user.reference$files )) {
                      showshinyalert(session, "shinyalertSelectReference",  
                                     HTML(paste(HANDLEFT, "Press 'Build the 
                                                reference' to start the RMA 
                                                reference building.")),
                                     styleclass = "info")
                    
                      return(0)
                    } else {
                      if (is.null(normalized.data) || 
                          any(fileInfo$name != attr(normalized.data, "files"))) {
                        showshinyalert(session, "shinyalertSelectReference",
                                       HTML("Press 'normalize files' to start 
                                            the RMA normalization. <br/> You can 
                                            download the established reference 
                                            by clicking 'Download reference for 
                                            later use.'"),
                                       styleclass = "info")  
                      return(1)
                      }
                    }
                  }
                }
              }
            }
          }}
          if (input$ChooseMethod == "upload") {
            if (is.null(input$refUpload)) {
              showshinyalert(session, "shinyalertSelectReference",  
                             HTML("Please upload a reference."),
                             styleclass = "info")  
              return(0)
            } else {
              if (file_ext(input$refUpload$name) != "rds") {
                wrong.file.text <- HTML(
                  paste(ERROR, "The uploaded file is not an 'rds' file. <br/> 
                        Please upload a correct reference")
                )
                showshinyalert(session, "shinyalertSelectReference",  
                               wrong.file.text,
                               styleclass = "danger")
                return(0)
              }
              RefUpload()
              if (!is.null(attributes(ref)) && 
                  "Version" %in% names(attributes(ref)) && 
                  attr(ref, "Version") %in% "hemaClass") {
                showshinyalert(session, "shinyalertSelectReference",  
                               "Press 'normalize files' to start the RMA 
                               normalization.",
                               styleclass = "info")  
                return(1)            
              } else {   
                showshinyalert(session, "shinyalertSelectReference",  
                               HTML(paste(ERROR, "The uploaded file cannot be 
                                          used as a reference. <br/> Please 
                                          upload a correct reference.")),
                               styleclass = "danger")
                return(0)
              }
            }
          }
          if (input$ChooseMethod == "standardReference") {
            showshinyalert(session, "shinyalertSelectReference",  
                           HTML(paste("Select the cohort you want to use as a 
                                      reference",
                                      "", "The possible references are:",
                                      "&#160 LLMPP CHOP:",
                                      "&#160 LLMPP R-CHOP:",
                                      "&#160 IDRC:",
                                      "&#160 MDFCI:",
                                      "&#160 CHEPRETRO:",
                                      "&#160 UAMS:",
                                      "", "When chosen:",
                                      "Press 'normalize files' to start the RMA 
                                      normalization.",
                                      sep = "<br/>")),
                           styleclass = "info")  
            return(1)
          }
          
          if (input$ChooseMethod == "RMA") {
            showshinyalert(session, "shinyalertSelectReference",  
                           HTML("Press Normalize files to pre-process the 
                                 uploaded CEL files according to the RMA 
                                 method"),
                           styleclass = "info")  
            return(1)
          }
          
          return(1)
        }
      }
    } else {
      return(0)
    }
  })
  
  output$showDownloadRefButton <- reactive({
    if (verbose) cat("showDownloadRefButton called.\n")
    
    createReference()
    ifelse(!is.null(user.reference), 1, 0)
  })
  
  
  output$showBuildRefButton <- reactive({
    if (verbose) cat("showBuildRefButton called.\n")
    
    input$refFiles
    
    if (input$ChooseMethod != "build") {
      return(0) # don't show
    }
    if (is.null(input$refFiles) || length(input$refFiles$name) < 2) {
      return(0)
    }
    return(1)
    
  })
  
  output$showErrorprints <- reactive({
    if (verbose) cat("showErrorprints called.\n")
    
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
  
  
  ##############################################################################
  ##
  ## Generate results
  ##
  ##############################################################################
  
  
  output$results <- renderDataTable({ 
    
    classify() # Classify
    
    if (length(chosen.names) == 0) {
      return(NULL)
    }
    
    results2 <- results[, c("files", chosen.names)]
    if (length(names(results2)) > 0) {
      for (i in names(results2)) {
        if (class(results2[,i]) == "numeric") {
          results2[,i] <- round(results2[,i], 3)
        }
      }
      return(results2)
    } else { 
      return(NULL)
    }
  })
  
  output$normalizedData <- renderDataTable({ 
    createData()
    
    if (is.null(normalized.data))
      return(NULL)
    
    data.frame(Probeset = row.names(normalized.data), 
               normalized.data)[1:100, , drop = FALSE]
  })
  
  output$rle.stats <- renderDataTable({ 
    createData()
	
    if (is.null(normalized.data.rle.stats))
      return(NULL)
	data.frame(
		"Files"=row.names(normalized.data.rle.stats),
		round(normalized.data.rle.stats[,1:3, drop = FALSE],3),
		"Include"=as.character(normalized.data.rle.stats[,3] < input$rle.iqr.threshold),
		check.names=FALSE)
		#"Include"=factor(normalized.data.rle.stats[,3] < input$rle.iqr.threshold, labels=c("No","Yes")),
  })
  
  # Render table of uploaded files
  output$usrFiles <- renderTable({  
    # input$celFiles will be NULL initially. After the user selects and uploads 
    # a file, it will be a data frame with 'name', 'size', 'type', and 
    # 'datapath' columns. The 'datapath' column will contain the local filenames 
    # where the data can be found.
    usrFiles <- input$usrFiles
    
    if (is.null(usrFiles)) {
      return(NULL)
    }
    return(usrFiles)    
  })
  
  # Download handler for results
  output$downloadData <- downloadHandler(
    filename = paste("HemaClass-Classifications", Sys.Date(), ".txt", sep = ""),
    content = function(file) {
      write.table(results, file, sep = "\t", quote = FALSE, row.names = FALSE)
	  #write.table(results[, c("files", chosen.names)], file, sep = "\t", quote = FALSE, row.names = FALSE)
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
  
  ##############################################################################
  ##
  ## HTML Pages
  ##
  ##############################################################################
  
  observeEvent(input$linkHelp, ({
    updateTabsetPanel(session, "nlp", selected = "Help")
  }))
  
  output$mpContent <- renderUI({
    input$nlp
    if (!is.null(input$nlp)) {
      pkg.version <- 
        paste0("<p>The server is running ", 
               '<a href = "https://github.com/oncoclass/hemaClass"',
               "<strong>hemaClass</strong> v", packageVersion("hemaClass"),
               "</p>")
      about <- includeHTML("www/About.html")
      switch(input$nlp,
             "Welcome"      = includeHTML("www/hemaClass.html"),
             "News"         = includeHTML("www/News.html"),
             "Help"         = includeHTML("www/howto.html"),
             "Publications" = includeHTML("www/Papers.html"),
             "About"        = HTML(paste(about, pkg.version)),
             NULL)  # If not matched
    }
  })
  
})
