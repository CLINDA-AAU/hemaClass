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

# Create patient summary template
jumbotron <- function(title = "Hello, world!", 
                      text = "...",
                      col = "#AAAAAA") {
  HTML(
    paste0(
      '<div style="background-color: ', col, '!important" class="jumbotron">
      <h2>', title, '</h2>
      <p>', text, '</p>
      </div>'
    )
  )
}
  
# Debug
verbose <- TRUE

#
# Run the shiny server
#

shinyServer(function(input, output, session) {
  if (verbose) cat("Shinyserver started.\n")
  
  addResourcePath('data', system.file("extdata/celfiles", package = "hemaClass"))
  
  # Initialzing the local objects to be filled
  normalized.data <- NULL
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
  
  output$downloadTestData <- downloadHandler(
    filename = function() {
      paste("cellll", "CEL", sep = ".")
    },
    content = function(file) {
      file.copy(' ', file)
    }
  )
  
  ##############################################################################
  ##
  ## Create metadata manually
  ##
  ##############################################################################
  
  
  observe({
    currentMetadataManual()
    
    output$hotableMetadataManual <- renderHotable({ 
      
      input$IPIcalc # should IPI be calculated
      
      fileInfo <- input$usrFiles
      
      if (!is.null(fileInfo$name)) {
        new.names <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
        if (input$IPIcalc) {
          old.data <- currentMetadataManual()

          cols <- c("CEL.files", "Age", "ECOG", "LDH", "N.Extra.Nodal", 
                    "Stage", "IPI", input$Additionalcolumns)
          
          data <- matrix(NaN, ncol =  length(cols), 
                         nrow = length(fileInfo$name), 
                         dimnames = list(new.names, cols))
          
          data <- as.data.frame(data)
          data$CEL.files <- new.names      
          
          if (!is.null(old.data) && !any(is.na(old.data$CEL.files))) {
            int.names <- intersect(colnames(old.data), colnames(data))
            int.cels  <- intersect(rownames(old.data), rownames(data))
            
            if (length(int.names) && length(int.cels)) {
              for (name in int.names) {
                data[int.cels, name] <- old.data[int.cels, name]
              }
            }
  
            IPI <- IPIreactive()
            data[names(IPI), "IPI"] <- IPI
          }
          
          as.data.frame(data)
          
        } else {    
          
          old.data <- currentMetadataManual()

          cols <- c("CEL.files", "IPI", input$Additionalcolumns)
          
          data <- matrix(NaN, ncol =  length(cols), 
                         nrow = length(fileInfo$name), 
                         dimnames = list(new.names, cols))
          data <- as.data.frame(data)
          data$CEL.files <- new.names
          
          if (!is.null(old.data) && !any(is.na(old.data$CEL.files))) {
            int.names <- intersect(colnames(old.data), colnames(data))
            int.cels  <- intersect(rownames(old.data), rownames(data))
          
            if (length(int.names) && length(int.cels)) {
              for (name in int.names) {
                data[int.cels, name] <- old.data[int.cels, name] 
              }
            }
          }
          as.data.frame(data)
        }
      } else {
        cols <- c("CEL.files", "IPI", input$Additionalcolumns)
        
        data <- matrix(NaN, ncol =  length(cols), nrow = length(fileInfo$name), 
                       dimnames = list(fileInfo$name, cols))
        as.data.frame(data)
      }
    }, readOnly = FALSE)
    
  })
  
  currentMetadataManual <- reactive({ 
    if (verbose) cat("currentMetadataManual called.\n")
    old.data <- NULL

    fileInfo <- (input$usrFiles) 
    
    if (!is.null(fileInfo$name)) {
      old.data <- hot.to.df(input$hotableMetadataManual)  
      if (!is.null( old.data )) {
        if ("CEL.files" %in% colnames(old.data)) {
          if (any(is.na(old.data$CEL.files))) {
            return(NULL)
          }
          new.names <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
          
          old.data$CEL.files[is.na(old.data$CEL.files)] <- 
            paste("a", 1:sum(is.na(old.data$CEL.files)))
          
          rownames(old.data) <- old.data$CEL.files
          old.data <- old.data[new.names, , drop = FALSE] 
        }
      } else {
        cols <- c("CEL.files", "IPI", input$Additionalcolumns)
        new.names <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
        data <- matrix(NA, ncol =  length(cols), nrow = length(fileInfo$name), 
                       dimnames = list(new.names, cols))
        data <- as.data.frame(data)
        data$CEL.files <- new.names
        
        old.data <- as.data.frame(data)
      }
      createData() 
      if (!is.null(normalized.data)) {
        chosenDataset <<- "Manually input metadata"
      }   
    }
    old.data 
  })
  
  
  IPIreactive <- reactive({   
    if (verbose) cat("IPIreactive called.\n")
    dataC <- currentMetadataManual()
    
    if (!all(c("Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage") %in% 
             colnames(dataC)) && !any(is.na(dataC$CEL.files))) {
      
      IPI <- rep(NaN, nrow(dataC))
    } else {
      
      a <- ifelse(dataC$Age            >   as.numeric(input$AGE.cut), 1, 0)
      b <- ifelse(dataC$ECOG           >   as.numeric(input$ECOG.cut), 1, 0)
      c <- ifelse(dataC$N.Extra.Nodal  >   as.numeric(input$N.Extra.Nodal.cut), 1, 0)
      d <- ifelse(dataC$Stage          >   as.numeric(input$Stage.cut), 1, 0)
      e <- ifelse(dataC$LDH            >   as.numeric(input$LDH.cut), 1, 0)
      
      IPI <- rowSums(data.frame(a, b, c, d, e)) 
      
      names(IPI) <- dataC$CEL.files
      
    } 
    IPI
  })
  
  
  output$downloadMetadataManual <- downloadHandler(
    filename = paste0("HemaClass-Metadata", Sys.Date(), ".xls"),
    content = function(file) {
      isolate({
        dataC <- currentMetadataManual()
      })
      WriteXLS::WriteXLS("dataC", ExcelFileName = file)
    }
  )
  
  
  ##############################################################################
  ##
  ## Read metadata
  ##
  ##############################################################################
  
  uploadMetaData <- reactive({
    if (verbose) cat("uploadMetaData called.\n")
    hideshinyalert(session, "shinyalertUploadMetaData")
    metadata.upload <<- NULL
    if (!is.null(input$usrMeta)) {
      if (grepl("rds", file_ext(input$usrMeta$name), ignore.case = TRUE)) {
        metadata.upload <<- readRDS(input$usrMeta$datapath) 
      }
      if (grepl("RData", file_ext(input$usrMeta$name), ignore.case = TRUE)) {
        load(input$usrMeta$datapath, ex <- new.env())
        name <- ls(ex)[1]
        metadata.upload <<- ex[[name]]
      }
      
      if (grepl("txt", file_ext(input$usrMeta$name), ignore.case = TRUE)) {
        if (input$ExttxtSep == "Other") {
          sep <- input$ExttxtSepOther
        } else {
          sep <- input$ExttxtSep
        }
        metadata.upload <<- read.table(input$usrMeta$datapath, 
                                       header = input$ExttxtHeader, sep = sep)
      }
      
      if (grepl("xls", file_ext(input$usrMeta$name), ignore.case = TRUE)) {
        names <- sheetNames(input$usrMeta$datapath)
        if (input$ExtXLSsheet %in% names || 
            input$ExtXLSsheet %in% as.character(1:length(names))) {
          metadata.upload <<- gdata::read.xls(input$usrMeta$datapath, 
                                              sheet = input$ExtXLSsheet)
        } else {
          showshinyalert(session, "shinyalertUploadMetaData",  
                         HTML("The selected sheet could not be found in the 
                              metadata!"),
                         styleclass = "danger") 
          return(NULL)
        }
      } else {
        showshinyalert(session, "shinyalertUploadMetaData",  
                       HTML("The choosen file is not of a supported type! 
                            Please upload a file of a supported type."),
                       styleclass = "danger")  
      }
    } else {
      showshinyalert(session, "shinyalertUploadMetaData",  
                     HTML(paste("Upload the file containing metadata. 
                                The supported file types are:",
                                "&#160.rds", "&#160.RData", 
                                "&#160.txt", "&#160.xls", 
                                sep = "<br/>")),
                     styleclass = "info")  
    }
  })
  
  output$MetaUploadCelFileNames <- renderUI({
    uploadMetaData()
    selected <- NULL
    if (is.null(metadata.upload)) {
      return(NULL)
    }
    
    if (!is.null(input$usrFiles))  {  
      
      fileInfo <- (input$usrFiles) 
      
      new.names <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
      
      
      if (any(apply(metadata.upload, 2, FUN = function(x) 
        any( gsub("\\.CEL$", "", x, ignore.case = TRUE) %in% new.names)))) {
        
        names <- apply(metadata.upload, 2, FUN = function(x) 
          any( gsub("\\.CEL$", "", x, ignore.case = TRUE) %in% new.names))      
        
        selected <- names(names)[names][1]
      }
    }
    list(
      selectInput(inputId = "metadataUploadCelfiles", 
                  "Column containg CEL file names", 
                  colnames(metadata.upload), selected = selected)
    )    
  })
  
  output$uploadMetaData <- renderDataTable({ 
    uploadMetaData()
    if (is.null(metadata.upload)) {
      return(NULL)
    }
    
    showReadMetaMethodsExtFun()
    if (class(metadata.upload) %in% c("data.frame", "matrix")) {
      as.data.frame(metadata.upload)
    } else {
      showshinyalert(session, "shinyalertUploadMetaData",  
                     HTML("The uploaded metafile could was neither a data.frame 
                          or matrix."),
                     styleclass = "danger")  
    }
  })
  
  showReadMetaMethodsExtFun <- reactive({
    if (verbose) cat("showReadMetaMethodsExtFun called.\n")
    if (is.null(input$usrMeta)) {
      return("none")
    }
    tolower(file_ext(input$usrMeta$name))
  })
  
  output$showReadMetaMethods <- reactive({
    if (verbose) cat("showReadMetaMethods called.\n")
    showReadMetaMethodsExtFun()
  })
  
  output$showReadMetaPrint <- reactive({
    if (verbose) cat("showReadMetaPrint called.\n")
    0
  })
  
  outputOptions(output, "showReadMetaPrint",   suspendWhenHidden = FALSE)
  outputOptions(output, "showReadMetaMethods", suspendWhenHidden = FALSE)
  
  metadataUploaded <- reactive({
    if (verbose) cat("metadataUploaded called.\n")
    fileInfo <- input$usrFiles
    uploadMetaData()
    if (!is.null(metadata.upload)) {
      if (any(duplicated(metadata.upload[, input$metadataUploadCelfiles]))) {
        showshinyalert(session, "shinyalertUploadMetaData",  
                       HTML("The column selected to include .CEL file 
                             names includes duplicates. <br/> Please choose 
                             another column or upload a new dataset"),
                       styleclass = "danger") 
      } else {
        new.names <- 
          gsub("\\.CEL$", "", metadata.upload[, input$metadataUploadCelfiles], 
               ignore.case = TRUE)
        rownames(metadata.upload) <-  new.names
        isolate({
          fileInfo <- input$usrFiles
        })
        new.names2 <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
        metadata.upload <- metadata.upload[new.names2, , drop = FALSE] 
        chosenDataset <<- "Uploaded metadata"
      }
    }
    metadata.upload
  })
  
  
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
    
    # LoadAnnotation()
    
    if (dataset %in% buildin.datasets) {
      if (!dataset %in% names(buildin.data)) {
        hideshinyalert(session, "shinyalertResults")
        GEP.file <- dir(file.path("Database/", dataset, "GEP"), 
                        full.names = TRUE, pattern = ".rds")
        GEP.data.temp <- readRDS(GEP.file)
        GEP <- microarrayScale(exprs(GEP.data.temp))
        colnames(GEP) <- gsub("\\.CEL$", "", colnames(GEP),
                              ignore.case = TRUE)
        buildin.data[[dataset]][["GEP"]] <<- GEP
        
        GEP.mean <- microarrayScale(exprs(GEP.data.temp), center = "mean")
        colnames(GEP) <- gsub("\\.CEL$", "", colnames(GEP.mean), 
                              ignore.case = TRUE)
        buildin.data[[dataset]][["GEP.mean"]] <<- GEP.mean
        
        
        Meta.file <- dir(file.path("Database/", dataset, "Metadata"), 
                         full.names = TRUE, pattern = ".rds")
        meta <- readRDS(Meta.file)  
        rownames(meta) <- gsub("\\.CEL$", "", rownames(meta),
                               ignore.case = TRUE)
        buildin.data[[dataset]][["metadata"]] <<- meta
      }
    }      
    return(buildin.data)
  })
  
  
  output$buildinMetaData <- renderDataTable({ 
    #input$buildindataselector
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
  
  ##############################################################################
  ##
  ## The used metadata
  ##
  ##############################################################################
  
  output$metadataselector <- renderUI({
    
    available.datasets <- vector()
    
    metadata.upload <- metadataUploaded() 
    if (!is.null(metadata.upload)) {
      available.datasets <- c(available.datasets, "Uploaded metadata")
    }
    metadata.manual <- currentMetadataManual()
    if (!is.null(metadata.manual)) {
      available.datasets <- c(available.datasets, "Manually input metadata")
    }
    metadata.buildin <- buildInMetadata()
    if (!is.null(metadata.buildin)) {
      available.datasets <- c(available.datasets, "Build-in dataset")
    }
    data.list <- list()
    
    for (data.iter in c( available.datasets)) {
      data.list[[data.iter]] <- data.iter
    }
    
    if (!is.null(chosenDataset) && chosenDataset %in% available.datasets) {
      selectInput(inputId  = "chooseMetaDataset", 
                  label    = "Choose a dataset",
                  choices  = data.list,
                  selected = chosenDataset)
    } else {
      selectInput(inputId  = "chooseMetaDataset", 
                  label    = "Choose a dataset",
                  choices  = data.list)
    }
  })
  
  
  outputOptions(output, "metadataselector", suspendWhenHidden = FALSE)
  
  MetaDataInUse <- reactive({
    if (verbose) cat("MetaDataInUse called.\n")
    
    fileInfo <- input$usrFiles
    metadata.in.use <- NULL
    if (!is.null(input$chooseMetaDataset)) {
      if (input$chooseMetaDataset == "Uploaded metadata") {
        metadata.in.use <- metadataUploaded()
      }
      if (input$chooseMetaDataset == "Manually input metadata") {
        metadata.in.use <- currentMetadataManual()
      }
      if (input$chooseMetaDataset == "Build-in dataset") {
        metadata.in.use <- buildInMetadata()
      }
    }
    
    return(metadata.in.use)
  })
  
  output$MetaDataInUseOut <- renderDataTable({   
    metadata.in.use <- MetaDataInUse()
    as.data.frame(metadata.in.use)
  })
  
  
  GEPInUse <- reactive({ 
    if (verbose) cat("GEPInUse called.\n")
    
    input$chooseMetaDataset
    MetaDataInUse()
    GEP.in.use <- NULL    
    
    if (!is.null(input$chooseMetaDataset)) {
      if (input$chooseMetaDataset == "Build-in dataset") {
        dataset <- input$choosebuildidDataset
        GEP.in.use <- as.data.frame(buildin.data[[dataset]][["GEP"]])
        return(GEP.in.use)
      }
    }
    
    createData() 
    if (!is.null(normalized.data)) {
      GEP.in.use <- normalized.data
      return(GEP.in.use)
    }   
    return(GEP.in.use)
  })
  
  GEPInUse2 <- reactive({ 
    if (verbose) cat("GEPInUse2 called.\n")
    
    MetaDataInUse()
    GEP.in.use <- NULL    
    
    if (!is.null(input$chooseMetaDataset)) {
      if (input$chooseMetaDataset == "Build-in dataset") {
        dataset <- input$choosebuildidDataset
        GEP.in.use <- as.data.frame(buildin.data[[dataset]][["GEP.mean"]])
        return(GEP.in.use)
      }
    }
    
    createData() 
    if (!is.null(normalized.data.mean)) {
      GEP.in.use <- normalized.data.mean
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
        if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))) {
          
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
      if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))) {
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
          gsub("\\.CEL$", "", input$usrFiles$name, ignore.case = TRUE)
        colnames(normalized.data.RMA$exprs.sc) <<-
          gsub("\\.CEL$", "", input$usrFiles$name, ignore.case = TRUE)
        
        normalized.data <<- normalized.data.RMA$exprs.sc
        attr(normalized.data, "files") <<- input$usrFiles$name
        
        normalized.data.mean <<- normalized.data.RMA$exprs.sc.mean
        attr(normalized.data.mean, "files") <<- input$usrFiles$name
        
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
    
    metadata.in.use <- MetaDataInUse()
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
    
    Dex <- DexamethasoneClassifierR()
    results$"DexProb"  <<- Dex$probs
    results$"DexClass" <<- Dex$class
    if ("Dexamethasone (P)" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "DexProb", "DexClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("DexProb", "DexProb"))
    }
    
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
  ## Patient summaries
  ##
  ##############################################################################
  
  # Handle I/O warnings/info in shinyalertPatientSummaries
  observe({
    # Show warning if uploaded and processed files are missing
    if (is.null(input$patientSummarySelectW) && is.null(input$usrFiles)) {
      no.uploaded.celfiles.text2 <- HTML(
        paste(WARNING, "You need to upload .CEL and normalize files first to 
              get patients summaries. You can do this 
              under the <strong>Load data</strong>", TO, "<strong>CEL 
              files</strong> tab.")
      )
      showshinyalert(session, "shinyalertPatientSummaries", 
                     no.uploaded.celfiles.text2, 
                     styleclass = "warning")
    } 

    # Show info if no chosen patients
    if (is.null(input$patientSummarySelectW) && 
        input$normalizeButton > 0) {
      showshinyalert(session, "shinyalertPatientSummaries",  
                     HTML(paste(HANDLEFT, "Choose one or more patients to 
                                  summarize.")),
                     styleclass = "info")
    } 

    # Hide if patients are chosen
    if (!is.null(input$patientSummarySelectW)) {
      hideshinyalert(session, "shinyalertPatientSummaries")
    }
  })
  
  
  # Generate and output prognosis text
  output$patientSummaries <- renderUI({  
    if (verbose) cat("patientSummaries text output created\n")
    
    input$patientSummarySelectW
    input$patientSummaryIPIW
    classify()
    
    # Organize data
    rownames(results) <- results$files
    metadata.in.use <- MetaDataInUse()
    
    if (!is.null(input$patientSummaryIPIW) && 
        input$patientSummaryIPIW != "Choose" && 
        input$patientSummaryIPIW %in% colnames(metadata.in.use)) {
      metadata.in.use$IPI <- metadata.in.use[, input$patientSummaryIPIW]   
    } else {
      metadata.in.use$IPI <- NA
    }
    
    # Create text for each selected patient
    prog.list <- list()
    for (i in seq_along(input$patientSummarySelectW)) {
      patient <- input$patientSummarySelectW[i]
      if (is.na(metadata.in.use[patient, "IPI"])) {
        ipi.text <- paste0(" and the patient have an unknown IPI score.")
      } else {
        ipi.text <- paste0(" and the patient have an IPI score of ", 
                           metadata.in.use[patient, "IPI"], ".")
      }
      patient.text <- 
        paste0("The cancer is of ", 
               results[patient, "ABCGCB2"], 
              " type",
              ipi.text)
      
      prog.list[[patient]] <- 
        jumbotron(title = paste("Patient", patient), 
                  text = patient.text,
                  col = rep(input$SelectedColoursPSw, 40)[i])
    }

    return(prog.list)
  })

  
  output$patientSummaryIPI <- renderUI({
    metadata.in.use <- MetaDataInUse()
    
    select <- NULL
    if (any(colnames(metadata.in.use) == "ipi")) {
      select <- "ipi"
    }
    if (any(colnames(metadata.in.use) == "IPI")) {
      select <- "IPI"
    }
    selectInput(inputId = "patientSummaryIPIW", "Column containing IPI", 
                c("Choose", colnames(metadata.in.use)), selected = select)
  })
  
  # Create patient selection box
  output$patientSummarySelect <- renderUI({
    classify()
    select2Input(inputId = "patientSummarySelectW", 
                 label = strong("Select patients to summarize:"), 
                 choices = results$files, 
                 selected = NULL) #results$files[1])
  })

  # Prognosis fit for predicted survival curves
  prognosisR <- reactive({
    if (verbose) cat("prognosisR called.\n")
    
    input$patientSummaryIPIW
    input$patientSummarySelectW
    
    isolate({
      
      classify()
      
      rownames(results) <- results$files
      metadata.in.use <- MetaDataInUse()

      prog.surv <- NULL
      stopifnot(all(rownames(results) == rownames(metadata.in.use))) 
      if (!is.null(input$patientSummarySelectW)) {
        
        prog.surv <- list()
        pred.data <- as.data.frame(cbind(metadata.in.use, results))      
        
        fit.OS  <<- readRDS("Database/Classification/fit.ABCGCB2.OS.rds")
        fit.PFS <<- readRDS("Database/Classification/fit.ABCGCB2.PFS.rds")
        metadataCombined <<- 
          readRDS("Database/Classification/metadataCombined.rds")
        
        if (!is.null(input$patientSummaryIPIW) && 
            input$patientSummaryIPIW != "Choose") {
          pred.data$IPI <- pred.data[, input$patientSummaryIPIW]   
        } else {
          pred.data$IPI <- NA
        }
        pred.data$IPI <- as.character(pred.data$IPI)
        pred.data$IPI[is.na(pred.data$IPI)] <- "NC"
        
        prog.surv[["Survfit.PFS"]] <- 
          survfit(fit.PFS, 
                  newdata = pred.data[input$patientSummarySelectW, , drop = FALSE], 
                  censor = FALSE)
        prog.surv[["Survfit.OS"]]  <- 
          survfit(fit.OS,  
                  newdata = pred.data[input$patientSummarySelectW, , drop = FALSE], 
                  censor = FALSE)
      
      }
      return(prog.surv)
      
    })
  })
  
  # Plot preducted survival curves
  output$patientSummaryPlot <- renderPlot({
    prog.surv <- prognosisR()
    
    if (is.null(prog.surv)) {
      return(NULL)
    }
    if (is.null(input$SelectedColoursPSw)) {
      return(NULL)
    }
    par(mfrow = c(1, 2))
    plot(prog.surv[["Survfit.PFS"]],
         xlab = "Years", ylab = "Survival", main = "Progression free survival",
         col = input$SelectedColoursPSw)
    legend("bottomleft", fill = rep(input$SelectedColoursPSw, 50), 
           legend = input$patientSummarySelectW, bty = "n", bg = "#FFFFFF40")
    plot(prog.surv[["Survfit.OS"]],
         xlab = "Years", ylab = "Survival", main = "Overall survival",
         col = input$SelectedColoursPSw)
  })
  
  
  # Color selector
  observe({ 
    input$SelectColourPS
    output$SelectedColoursPS <- renderUI({
      isolate({
        new.col <- ifelse(input$jscolorInputPS == "#FFFFFF", "#AAAAAA", 
                          input$jscolorInputPS) 
        selected.colors <- c(input$SelectedColoursPSw, new.col)
        
        return(
          select2Input(inputId = "SelectedColoursPSw", 
                       label = "The selected colours", 
                       selected = selected.colors)
        )
      })
    })   
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
      showshinyalert(session, "shinyalertInputMeta", no.uploaded.celfiles.text, 
                     styleclass = "warning")
      showshinyalert(session, "shinyalertUploadMeta", no.uploaded.celfiles.text, 
                     styleclass = "warning")
      showshinyalert(session, "shinyalertResults", no.uploaded.celfiles.text, 
                     styleclass = "warning")
      return(0)
    } else {
      fileInfo <- input$usrFiles
      if (!all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))) {
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
        hideshinyalert(session, "shinyalertInputMeta")
        hideshinyalert(session, "shinyalertUploadMeta")
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
      if (all(grepl("\\.CEL$", fileInfo$name, ignore.case = TRUE))) {
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
              if (!all(grepl("\\.CEL$", input$refFiles$name, 
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
      switch(input$nlp,
             "Welcome"      = includeHTML("www/hemaClass.html"),
             "News"         = includeHTML("www/News.html"),
             "Help"         = includeHTML("www/howto.html"),
             "Publications" = includeHTML("www/Papers.html"),
             "About"        = includeHTML("www/About.html"),
             NULL)  # If not matched
    }
  })
  
})
