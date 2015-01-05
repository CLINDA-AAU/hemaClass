#install.packages("devtools")
#devtools::install_github("shiny", "rstudio")
#devtools::install_github("shiny-incubator", "rstudio")
library(shiny)
library(tools)
library(hemaClass)
#devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
library(gdata)
require(survival)
require(WriteXLS)
# Changing maximum file size
options(shiny.maxRequestSize = 30*1024^2)


# 
# 
# PlotKM <- function(index, surv.object,
#                    cut.points   = c(1/3, 2/3),
#                    our.colscheme = c("black", "darkgrey", "red"),
#                    lty      = c(1, 1, 1),
#                    main      = "",
#                    ylab = "",
#                    xlab = "",
#                    ...) {
#   
#   # Function to perform KM plot
#   
#   threshold <- cut(index,
#                    c(min(index) - 1,
#                      cut.points,
#                      max(index) + 1 ))
#   
#   levels <- levels(threshold)
#   legend <- levels(threshold)
#   plot(survfit(surv.object ~ threshold),
#        col  = our.colscheme,
#        lwd  = 2,
#        xlab = xlab,
#        ylab = ylab,
#        main = main, ...)
#   
#   logRankTest <- survdiff(surv.object ~ threshold)
#   
#   nchar <- max(nchar(legend)) - nchar(legend) + 1
#   spaces <- vector()
#   
#   for(i in 1:length(legend)){
#     spaces[i] <- paste(rep(" ", nchar[i]), sep = "", collapse = "")
#   }
#   
#   nchar <- max(nchar(summary(threshold))) - nchar(summary(threshold)) + 1
#   spaces2 <- vector()
#   
#   for(i in 1:length(legend)){
#     spaces2[i] <- paste(rep(" ", nchar[i]), sep = "", collapse = "")
#   }
#   
#   
#   old.par <- par(no.readonly = TRUE)
#   on.exit(par(old.par))
#   
#   par(family = 'mono')
#   legend("bottomleft",
#          legend = paste(legend, ",", spaces, "n = ", spaces2,
#                         summary(threshold), sep = ""),
#          bty   = "n",
#          col   = our.colscheme,
#          lty   = lty,
#          lwd   = 2)
#   
#   legend("bottomright",
#          bty = "n",
#          legend = paste("P-value = ",
#                         as.character(signif(1-pchisq(logRankTest$chisq, 1), 1)), sep = ""))
#   
# }
# 
# 
# 
# coxR <- function(formula = OS5 ~ probe, data){
#   
#   terms <- terms(formula)
#   
#   labels <- attr(terms, "term.labels")
#   
#   data <- data[!apply(data[, labels, drop = FALSE], 1, function(x) any(is.na(x))), ]
#   
#   
#   for(lab.iter in labels)
#     if(is.factor(data[,lab.iter]))
#       data[,lab.iter] <- droplevels(data[,lab.iter])
#   
#   for(lab.iter in labels)
#     if(is.character(data[,lab.iter]))
#       data[,lab.iter] <- as.factor(data[,lab.iter])
#   
#   
#   levels <- vector()
#   for(lab.iter in labels){
#     if(is.factor(data[,lab.iter]))
#       levels <- c(levels, levels(data[,lab.iter]))
#     if(!is.factor(data[,lab.iter]))
#       levels <- c(levels, lab.iter)
#     
#   }
#   
#   mat <- matrix(NaN, nrow = length(levels), ncol = 6)
#   
#   rownames(mat) <- levels
#   
#   colnames(mat) <- rep(c("Hazard ratio", "95% CI", "P-Value"), 2)
#   
#   mat[, c(1,4)] <- 1 
#   
#   y <- rownames(attr(terms,"factors"))[1]
#   
#   
#   for(lab.iter in labels){
#     formula2 <- 
#       as.formula((substitute(a ~ b, 
#                              list(a = as.name(rownames(attr(terms,"factors"))[1]), 
#                                   b = as.name(lab.iter)))))
#     
#     cox <- summary(coxph(formula2, data = data))
#     COEF <- cox$coefficients
#     
#     if(is.factor(data[,lab.iter]))
#       wh <- gsub(lab.iter, "", rownames(COEF))
#     
#     if(!is.factor(data[,lab.iter]))
#       wh <- rownames(COEF)
#     
#     mat[wh, 1] <- round(COEF[,2], 2) 
#     conf <-  round(cox$conf.int[, 3:4], 2)
#     
#     if(!is.null(dim(conf)))
#       mat[wh, 2] <- apply(conf, 1, function(x) 
#         paste("(",x[1], "-", x[2], ")", sep = ""))
#     
#     if(is.null(dim(conf)))
#       mat[wh, 2] <- 
#       paste("(",conf[1], "-", conf[2], ")", sep = "")
#     
#     mat[wh, 3] <- signif(cox$coefficients[,5], 2)
#   }
#   
#   
#   cox <- summary(coxph(formula, data = data))
#   COEF <- cox$coefficients
#   
#   wh <- rownames(COEF)
#   for(lab.iter in labels)
#     if(is.factor(data[,lab.iter]))
#       wh <- gsub(lab.iter, "", wh)
#   
#   
#   mat[wh, 4] <- round(COEF[,2], 2) 
#   conf <-  round(cox$conf.int[, 3:4], 2)
#   
#   if(class(conf) == "numeric"){
#     mat[wh, 5] <-  paste("(",conf[1], "-", conf[2], ")", sep = "")
#   }else{
#     mat[wh, 5] <- apply(conf, 1, function(x) 
#       paste("(",x[1], "-", x[2], ")", sep = ""))
#   }
#   mat[wh, 6] <- signif(cox$coefficients[,5], 2)
#   
#   mat[mat == "NaN"] <- "-"
#   
#   list(n = paste("n = ", cox$n, ", number of events = ", 
#                  cox$nevent, sep = ""),
#        result = as.data.frame(mat, stringsAsFactors=FALSE))
#   
# }




#
# Run the shiny server
#
cat("done.\n")
shinyServer(function(input, output, session) {
  # Initialzing the normalized.data and results object
  normalized.data <- NULL
  normalized.data.RMA <- NULL
  user.reference  <- NULL
  user.reference.affy <- NULL
  user.affy <- NULL
  results <- list()
  
  LoadAnnotation <- reactive({
    HGU133Plus2 <<- readRDS("../Database/V1/Annotation/HGU133Plus2.na33.annot.RDSData")
  })
  
  buildin.data <- list()
  buildin.datasets <- setdiff(dir("../Database/V1/"), c("Annotation", "Classification"))
  
#   # hotable
#   
#   
#   
#   
#   my.lookUp <- reactive({
#     x <- input$geneKM
#     LoadAnnotation()
#     data = HGU133Plus2
#     what = "ALIAS2PROBE"
#     if(what == "ALIAS2PROBE"){
#       return.data <- data.frame()
#       xx <- strsplit(data$Gene.Symbol, " /// ")
#       
#       alias  <- unlist(xx)
#       reps   <- (unlist(lapply(xx, FUN = function(x) length(x))))   
#       probes <- rep(data[,1], reps)
#       
#       data2  <- data.frame(alias, probes)
#       data2  <- data2[data2$alias %in% x, ]
#       data2  <- data2[order(data2$alias),]  
#       data2$alias <- as.character(data2$alias)
#       rownames(data2) <- data2$probes
#     }
#     
#     return(data2)
#   })
#   
#   
#   output$probeselector <- renderUI({
#     
#     probe.data <- my.lookUp()
#     
#     if(nrow(probe.data) == 0)
#       return(NULL)
#     
#     probe.list <- list()
#     for(probe.iter in rownames(probe.data))
#       probe.list[[probe.iter]] <- probe.iter
#     
#     
#     list(
#       selectInput("AvailableProbesetsKM", "Choose a probeset",
#                   choices = probe.list)
#     )
#     
#   })
#   
#   
#   output$KMSurvival <- renderUI({
#     
#     metadata.in.use <- MetaDataInUse()
#     
#     list(
#       selectInput(inputId = "KMSurvivalTime",   "Column containing survival times",  colnames(metadata.in.use)),
#       selectInput(inputId = "KMSurvivalStatus", "Column containing survival status", colnames(metadata.in.use))
#     )    
#   })
  
  ##################################
  ##
  ## Create metadata manually
  ##
  ##################################
  
  
  #   output$hotableClinical <- renderHotable({  
  #     fileInfo <- input$usrFiles
  #     if(!is.null(fileInfo$name)){
  #       if(!exists("input$hotableClinical")){        
  #         data <- matrix(NaN, ncol =  6, nrow = length(fileInfo$name), 
  #                        dimnames = list(fileInfo$name, c("CEL.files", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage")))
  #         data <- as.data.frame(data)
  #         data$CEL.files <- fileInfo$name
  #         data
  #       }else{
  #         old.data <- hot.to.df(input$hotableClinical)
  #         data <- matrix(NaN, ncol =  6, nrow = length(fileInfo$name), 
  #                        dimnames = list(fileInfo$name, c("CEL.files", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage")))
  #         data <- as.data.frame(data)
  #         data$CEL.files <- fileInfo$name
  #         
  #         int <- intersect(old.data$CEL.files, data$CEL.files)
  #         
  #         if(length(int))
  #           data[int, ] <- old.data[int, ]
  #         data
  #       }
  #     }else{
  #       data <- matrix(NaN, ncol =  6, nrow = length(fileInfo$name), 
  #                      dimnames = list(fileInfo$name, c("CEL.files", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage")))
  #       as.data.frame(data)
  #     }
  #   }, readOnly = FALSE)
  #   
  #   
  #   
  #   output$hotableIPI <- renderHotable({ 
  #     
  #     input$IPIcalc
  #     
  #     df <- hot.to.df(input$hotableClinical)
  #     fileInfo <- (input$usrFiles)
  #     
  #     cols <- c("CEL.files", "IPI", input$Additionalcolumns)
  #     
  #     if(!is.null(fileInfo$name)){
  #       if(input$IPIcalc){
  #         
  #         cols <- c("CEL.files", "Age", "ECOG", "LDH", "N.Extra.Nodal", "Stage", "IPI", input$Additionalcolumns)
  #         
  #         old.data <- hot.to.df(input$hotableIPI)
  #         old.data$CEL.files[is.na(old.data$CEL.files)] <- paste("a", 1:sum(is.na(old.data$CEL.files)))
  #         
  #         rownames(old.data) <- old.data$CEL.files
  #         
  #         data <- matrix(NaN, ncol =  length(cols), nrow = length(fileInfo$name), 
  #                        dimnames = list(fileInfo$name, cols))
  #         
  #         data <- as.data.frame(data)
  #         data$CEL.files <- fileInfo$name      
  #         
  #         int.names <- intersect(colnames(old.data), colnames(data))
  #         int.cels <- intersect(rownames(old.data), rownames(data))
  #         if(length(int.names) && length(int.cels))
  #         for(name in int.names)
  #           data[int.cels, name] <- old.data[int.cels, name]
  #         
  #         if(! is.null(hot.to.df(input$hotableClinical))){
  #           IPI <- IPIreactive()
  #           
  #           data[names(IPI), "IPI"] <- IPI
  #         }
  #         
  #         as.data.frame(data)
  #         
  #       }else{    
  #         old.data <- hot.to.df(input$hotableIPI)
  #        # print(old.data)
  #        old.data$CEL.files[is.na(old.data$CEL.files)] <- paste("a", 1:sum(is.na(old.data$CEL.files)))
  #         rownames(old.data) <- old.data$CEL.files
  #         data <- matrix(NaN, ncol =  length(cols), nrow = length(fileInfo$name), 
  #                        dimnames = list(fileInfo$name, cols))
  #         data <- as.data.frame(data)
  #         data$CEL.files <- fileInfo$name
  #         
  #         int.names <- intersect(colnames(old.data), colnames(data))
  #         int.cels  <- intersect(rownames(old.data), rownames(data))
  #         
  #         if(length(int.names) && length(int.cels))
  #           for(name in int.names)
  #             data[int.cels, name] <- old.data[int.cels, name]
  #         
  #         as.data.frame(data)
  #       }
  #     }else{
  #       data <- matrix(NaN, ncol =  length(cols), nrow = length(fileInfo$name), 
  #                      dimnames = list(fileInfo$name, cols))
  #       as.data.frame(data)
  #     }
  #   }, readOnly = FALSE)
  
  
  
  
  output$hotableMetadataManual <- renderHotable({ 
    
    input$IPIcalc # should IPI be calculated
    
    fileInfo <- (input$usrFiles) 
    
    if(!is.null(fileInfo$name)){
      new.names <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
      if(input$IPIcalc){
        
        old.data <- currentMetadataManual()
        
        # print(old.data)
        cols <- c("CEL.files", "Age", "ECOG", "LDH", "N.Extra.Nodal", 
                  "Stage", "IPI", input$Additionalcolumns)
        
        data <- matrix(NaN, ncol =  length(cols), nrow = length(fileInfo$name), 
                       dimnames = list(new.names, cols))
        
        data <- as.data.frame(data)
        data$CEL.files <- new.names      
        
        if(!is.null(old.data) && !any(is.na(old.data$CEL.files))){
          int.names <- intersect(colnames(old.data), colnames(data))
          int.cels <- intersect(rownames(old.data), rownames(data))
          
          if(length(int.names) && length(int.cels))
            for(name in int.names)
              data[int.cels, name] <- old.data[int.cels, name]
          
          # if(!is.null(hot.to.df(input$hotableClinical))){
          IPI <- IPIreactive()
          
          data[names(IPI), "IPI"] <- IPI
          #}
        }
        
        as.data.frame(data)
        
      }else{    
        old.data <- currentMetadataManual()
        # print(old.data)
        cols <- c("CEL.files", "IPI", input$Additionalcolumns)
        
        data <- matrix(NaN, ncol =  length(cols), nrow = length(fileInfo$name), 
                       dimnames = list(new.names, cols))
        data <- as.data.frame(data)
        data$CEL.files <- new.names
        
        if(!is.null(old.data) && !any(is.na(old.data$CEL.files))){
          int.names <- intersect(colnames(old.data), colnames(data))
          int.cels  <- intersect(rownames(old.data), rownames(data))
          
          
          if(length(int.names) && length(int.cels))
            for(name in int.names)
              data[int.cels, name] <- old.data[int.cels, name]
        }
        as.data.frame(data)
      }
    }else{
      
      cols <- c("CEL.files", "IPI", input$Additionalcolumns)
      
      data <- matrix(NaN, ncol =  length(cols), nrow = length(fileInfo$name), 
                     dimnames = list(fileInfo$name, cols))
      as.data.frame(data)
    }
  }, readOnly = FALSE)
  
  
  currentMetadataManual <- reactive({   
    old.data <- NULL
    print(hot.to.df(input$hotableMetadataManual))
    isolate({
      fileInfo <- (input$usrFiles) 
    })
    
    
    if(!is.null(fileInfo$name)){
      old.data <- hot.to.df(input$hotableMetadataManual)  
      #print(old.data)
      if(!is.null( old.data ))
        if("CEL.files" %in% colnames(old.data)){
          
          if(any(is.na(old.data$CEL.files)))
            return(NULL)
          
          new.names <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
          
          old.data$CEL.files[is.na(old.data$CEL.files)] <- paste("a", 1:sum(is.na(old.data$CEL.files)))
          
          rownames(old.data) <- old.data$CEL.files
          old.data <- old.data[new.names, , drop = FALSE] 
        }
    }
    old.data 
  })
  
  
  IPIreactive <- reactive({   
    
    dataC <- currentMetadataManual()
    
    if(!all(c("Age", "ECOG", "LDH", "N.Extra.Nodal", 
              "Stage") %in% colnames(dataC)) && !any(is.na(dataC$CEL.files))){
      
      IPI <- rep(NaN, nrow(dataC))
      
    }else{
      
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
  
  
  ##################################
  ##
  ## Read metadata
  ##
  ##################################
  
  uploadMetaData <- reactive({
    
    hideshinyalert(session, "shinyalertUploadMetaData")
    metadata.upload <<- NULL
    if(!is.null(input$usrMeta)){
      if(grepl("rds", file_ext(input$usrMeta$name), ignore.case = TRUE)){
        metadata.upload <<- readRDS(input$usrMeta$datapath) 
      }
      if(grepl("RData", file_ext(input$usrMeta$name), ignore.case = TRUE)){
        load(input$usrMeta$datapath, ex <- new.env())
        name <- ls(ex)[1]
        metadata.upload <<- ex[[name]]
      }
      
      if(grepl("txt", file_ext(input$usrMeta$name), ignore.case = TRUE)){
        if(input$ExttxtSep == "Other"){
          sep <- input$ExttxtSepOther
        }else{
          sep <- input$ExttxtSep
        }
        metadata.upload <<- read.table(input$usrMeta$datapath, header = input$ExttxtHeader, sep = sep)
      }
      
      if(grepl("xls", file_ext(input$usrMeta$name), ignore.case = TRUE)){
        names <- sheetNames(input$usrMeta$datapath)
        if(input$ExtXLSsheet %in% names || input$ExtXLSsheet %in% as.character(1:length(names) )){
          metadata.upload <<- gdata::read.xls(input$usrMeta$datapath, sheet = input$ExtXLSsheet)
        }else{
          showshinyalert(session, "shinyalertUploadMetaData",  
                         HTML(paste("The selected sheet could not be found in the metadata!",
                                    sep="<br/>")),
                         styleclass = "error") 
          return(NULL)
        }
      }
    }
  })
  
  output$MetaUploadCelFileNames <- renderUI({
    uploadMetaData()
    selected <- NULL
    if(is.null(metadata.upload))
      return(NULL)
    
    
    if(!is.null(input$usrFiles))  {  
      
      fileInfo <- (input$usrFiles) 
      
      new.names <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
      
      
      if(any(apply(metadata.upload, 2, 
                   FUN = function(x) any( gsub("\\.CEL$", "", x, ignore.case = TRUE) %in% new.names)))){
        
        names <- apply(metadata.upload, 2, 
                       FUN = function(x) any( gsub("\\.CEL$", "", x, ignore.case = TRUE) %in% new.names))      
        
        selected <- names(names)[names][1]
      }
    }
    list(
      selectInput(inputId = "metadataUploadCelfiles", "Column containg CEL file names", colnames(metadata.upload), selected = selected)
    )    
  })
  
  output$uploadMetaData <- renderDataTable({ 
    
    uploadMetaData()
    if(is.null(metadata.upload))
      return(NULL)
    #metadata.upload <- metadata.upload[[1]]
    showReadMetaMethodsExtFun()
    if(class(metadata.upload) %in% c("data.frame", "matrix")){
      as.data.frame(metadata.upload)
    }else{
      showshinyalert(session, "shinyalertUploadMetaData",  
                     HTML(paste("The uploaded metafile could was neither a data.frame or matrix",
                                sep="<br/>")),
                     styleclass = "error")  
    }
    
  })
  
  
  
  
  showReadMetaMethodsExtFun <- reactive({
    if(is.null(input$usrMeta))
      return("none")
    tolower(file_ext(input$usrMeta$name))
  })
  
  output$showReadMetaMethods <- reactive({
    showReadMetaMethodsExtFun()
  })
  
  output$showReadMetaPrint <- reactive({
    0
  })
  
  outputOptions(output, "showReadMetaPrint", suspendWhenHidden = FALSE)
  
  outputOptions(output, "showReadMetaMethods", suspendWhenHidden = FALSE)
  
  
  
  metadataUploaded <- reactive({
    fileInfo <- input$usrFiles
    uploadMetaData()
    if(!is.null(metadata.upload)) 
      
      if(any(duplicated(metadata.upload[, input$metadataUploadCelfiles]))){
        showshinyalert(session, "shinyalertUploadMetaData",  
                       HTML(paste("The column selected to include .CEL file names include duplicates.",
                                  "Please choose another column or upload a new dataset",
                                  sep="<br/>")),
                       styleclass = "error") 
      }else{
        new.names <- gsub("\\.CEL$", "", metadata.upload[, input$metadataUploadCelfiles], 
                          ignore.case = TRUE)
        rownames(metadata.upload) <-  new.names
        isolate({
          fileInfo <- input$usrFiles
        })
        new.names2 <-  gsub("\\.CEL$", "", fileInfo$name, ignore.case = TRUE)
        metadata.upload <- metadata.upload[new.names2, , drop = FALSE] 
      }
    metadata.upload
  })
  
  
  ##################################
  ##
  ## Build in data
  ##
  ##################################
  
  
  output$buildindataselector <- renderUI({
    data.list <- list()
    
    for(data.iter in c("Choose", buildin.datasets))
      data.list[[data.iter]] <- data.iter
    
    selectInput(inputId = "choosebuildidDataset", 
                label   = "Choose a data set",
                choices = data.list)
    
  })
  
  
  loadbuildinData <- reactive({     
    
    if(is.null(buildin.data)){
      buildin.data <<- list()
    }
    dataset <- input$choosebuildidDataset
    
    if(is.null(dataset) || dataset == "Choose")
      return(buildin.data)
    
    LoadAnnotation()
    
    if(dataset %in% buildin.datasets){
      if(!dataset %in% names(buildin.data)){
        hideshinyalert(session, "shinyalertResults")
        GEP.file <- dir(file.path("../Database/V1/", dataset, "GEP"), full.names=TRUE, pattern = ".RDSData")
        GEP <- microarrayScale(exprs(readRDS(GEP.file)))
        colnames(GEP) <- gsub("\\.CEL$", "", colnames(GEP), ignore.case = TRUE)
        buildin.data[[dataset]][["GEP"]] <<- GEP
        
        
        Meta.file <- dir(file.path("../Database/V1/", dataset, "Metadata"), full.names=TRUE, pattern = ".RDSData")
        meta <- readRDS(Meta.file)  
        rownames(meta) <- gsub("\\.CEL$", "", rownames(meta), ignore.case = TRUE)
        buildin.data[[dataset]][["metadata"]] <<- meta
      }
    }      
    return(buildin.data)
  })
  
  
  output$buildinMetaData <- renderDataTable({ 
    #input$buildindataselector
    loadbuildinData()
    dataset <- input$choosebuildidDataset
    
    if(is.null(dataset) || dataset == "Choose")
      return(NULL)
    
    print(names(buildin.data))
    if(dataset %in% buildin.datasets)
      if(dataset %in% names(buildin.data)){
        print(buildin.data[[dataset]][["metadata"]])
        as.data.frame(buildin.data[[dataset]][["metadata"]])
      }
  })
  
  output$buildinGEP <- renderDataTable({ 
    # input$buildindataselector
    loadbuildinData()
    
    dataset <- input$choosebuildidDataset
    if(is.null(dataset) || dataset == "Choose")
      return(NULL)
    
    if(dataset %in% buildin.datasets)
      if(dataset %in% names(buildin.data))
        as.data.frame(buildin.data[[dataset]][["GEP"]]) 
  })
  
  
  buildInMetadata <- reactive({
    loadbuildinData()
    dataset <- input$choosebuildidDataset
    
    if(is.null(dataset) || dataset == "Choose")
      return(NULL)
    
    if(dataset %in% buildin.datasets)
      if(dataset %in% names(buildin.data)){   
        data <- as.data.frame(buildin.data[[dataset]][["metadata"]])
        #print("I was here")
        return(data)  
      } 
  })
  
  ##################################
  ##
  ## The used metadata
  ##
  ##################################
  
  output$metadataselector <- renderUI({
    
    available.datasets <- vector()
    
    metadata.upload <- metadataUploaded() 
    if(!is.null(metadata.upload))
      available.datasets <- c(available.datasets, "Uploaded metadata")
    
    metadata.manual <- currentMetadataManual()
    if(!is.null(metadata.manual))
      available.datasets <- c(available.datasets, "Manually input metadata")
    
    metadata.buildin <- buildInMetadata()
    if(!is.null(metadata.buildin))
      available.datasets <- c(available.datasets, "Build-in dataset")
    
    data.list <- list()
    
    for(data.iter in c( available.datasets))
      data.list[[data.iter]] <- data.iter
    
    selectInput(inputId = "chooseMetaDataset", 
                label   = "Choose a data set",
                choices = data.list)
  })
  
  
  outputOptions(output, "metadataselector", suspendWhenHidden = FALSE)
  
  MetaDataInUse <- reactive({ 
    fileInfo <- input$usrFiles
    metadata.in.use <- NULL
    if(!is.null(input$chooseMetaDataset)){
      if(input$chooseMetaDataset == "Uploaded metadata")
        metadata.in.use <- metadataUploaded()
      
      if(input$chooseMetaDataset == "Manually input metadata")
        metadata.in.use <- currentMetadataManual()
      
      
      if(input$chooseMetaDataset == "Build-in dataset")
        metadata.in.use <- buildInMetadata()
    }
    
    return(metadata.in.use)
  })
  
  output$MetaDataInUseOut <- renderDataTable({   
    metadata.in.use <- MetaDataInUse()
    as.data.frame(metadata.in.use)
  })
  
  
  GEPInUse <- reactive({ 
    MetaDataInUse()
    GEP.in.use <- NULL    
    
    if(!is.null(input$chooseMetaDataset))
      if(input$chooseMetaDataset == "Build-in dataset"){
        dataset <- input$choosebuildidDataset
        GEP.in.use <- as.data.frame(buildin.data[[dataset]][["GEP"]])
        return(GEP.in.use)
      }
    
    
    createData() 
    if (!is.null(normalized.data)) {
      GEP.in.use <- normalized.data
      return(GEP.in.use)
    }   
    
    return(GEP.in.use)
  })
  
  ##################################
  ##
  ## Upload .CEL files
  ##
  ##################################
  
  
  
  
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
        LoadAnnotation()
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
        colnames(normalized.data.RMA$exprs) <<- gsub("\\.CEL$", "", input$usrFiles$name, ignore.case = TRUE)
        colnames(normalized.data.RMA$exprs.sc) <<- gsub("\\.CEL$", "", input$usrFiles$name, ignore.case = TRUE)
        normalized.data <<- normalized.data.RMA$exprs.sc
        attr(normalized.data, "files") <<- input$usrFiles$name
        
        hideshinyalert(session, "shinyalertResults")
        showshinyalert(session, "shinyalertNormalizationSuccess",  HTML(paste("The normalization was successful!")),
                       styleclass = "success")
      }
    })
  })
  
  ############################
  ##
  ## The classification
  ##
  ############################
  
  # Function to that calls the classification procedures
  #   classify <- reactive({
  #     
  #     #createData()
  #     
  #     normalized.data <<- GEPInUse()
  #     
  #     results <<- list()
  #     
  #     # Create or update data if necessary
  #     
  #     if (is.null(normalized.data)) {
  #       return(NULL)
  #     }
  #     
  #     results$files <<- colnames(normalized.data)
  # 
  #     
  #     if ("BAGS" %in% input$getClassifications) {
  #       bags <- BAGS(normalized.data, cut.spec=0)
  #       results$ProbOfBAGS <<- bags$prob
  #       results$BAGS <<- bags$class
  #     } else {
  #       results <<- results[!(names(results) %in% c("ProbOfBAGS", "BAGS"))]
  #     }
  #     
  #     if ("ABCGCB" %in% input$getClassifications) {
  #       abcgcb <- ABCGCB(normalized.data, NC.range = input$nc.range)
  #       results$"ProbOfABC" <<- abcgcb$prob
  #       results$"ABCGCB" <<- abcgcb$class
  #     } else {
  #       results <<- results[!(names(results) %in% c("ProbOfABC", "ABCGCB"))]
  #     }
  #     
  #     av.drugs <- c("Cyclophosphamide", "Doxorubicin", "Vincristine") 
  #     drugs <<- input$getClassifications 
  #     drugs <- drugs[drugs %in% c(av.drugs, "Combined")]
  #     if (length(intersect(av.drugs,input$getClassifications)) > 0) {
  #       
  #       cut <- list(Cyclophosphamide = input$Cyclophosphamide.range,
  #                   Doxorubicin = input$Doxorubicin.range,
  #                   Vincristine = input$Vincristine.range,
  #                   Combined    = input$Combined.range)
  #       
  #       CHO <- ResistanceClassifier(normalized.data, drugs=drugs, cut = cut)
  #       
  #       if ("Cyclophosphamide" %in% input$getClassifications) {
  #         results$CycProb  <<- CHO$prob[,"Cyclophosphamide"]
  #         results$CycClass <<- CHO$class[,"Cyclophosphamide"]
  #       } else {
  #         results <<- results[!(names(results) %in% c("CycProb", "CycClass"))]
  #       }
  #       
  #       if ("Doxorubicin" %in% input$getClassifications) {
  #         results$DoxProb  <<- CHO$prob[,"Doxorubicin"]
  #         results$DoxClass <<- CHO$class[,"Doxorubicin"]
  #       } else {
  #         results <<- results[!(names(results) %in% c("DoxProb", "DoxClass"))]
  #       }
  #       if ("Vincristine" %in% input$getClassifications) {
  #         results$VinProb  <<- CHO$prob[,"Vincristine"]
  #         results$VinClass <<- CHO$class[,"Vincristine"]
  #       } else {
  #         results <<- results[!(names(results) %in% c("VinProb", "VinProb"))]
  #       }
  #       if ("Combined" %in% input$getClassifications & 
  #             length(drugs) > 2) {
  #         results$CombProb  <<- CHO$prob[,"Combined"]
  #         results$CombClass <<- CHO$class[,"Combined"]
  #       } else {
  #         results <<- results[!(names(results) %in% c("CombProb", "CombClass"))]
  #       }
  #     } else {
  #       results <<- results[!(names(results) %in% c("CycProb", "CycClass", "DoxProb", "DoxClass",
  #                                                   "VinProb", "VinProb","CombProb", "CombClass"))]
  #     }
  #     
  #     results <<- base::as.data.frame(results, row.names = NULL)
  #     rownames(results) <<- NULL
  #   })
  
  
  BAGSR <- reactive({
    normalized.data <- GEPInUse()
    BAGS(normalized.data, cut.spec=0)
  })
  
  ABCGCBR <- reactive({
    normalized.data <- GEPInUse()
    ABCGCB(normalized.data, NC.range = input$nc.range)
  })
  
  
  ResistanceClassifierR <- reactive({
    normalized.data <- GEPInUse()
    av.drugs <<- c("Cyclophosphamide", "Doxorubicin", "Vincristine") 
    drugs <<- input$getClassifications 
    drugs <<- drugs[drugs %in% c(av.drugs, "Combined")]
    
    
    cut <- list(Cyclophosphamide = input$Cyclophosphamide.range,
                Doxorubicin = input$Doxorubicin.range,
                Vincristine = input$Vincristine.range,
                Combined    = input$Combined.range)
    
    ResistanceClassifier(normalized.data, drugs=c(av.drugs, "Combined"), cut = cut)
  })
  
  classify <- reactive({
    
    #createData()
    
    normalized.data <<- GEPInUse()
    
    results <<- list()
    chosen.names <<- vector()
    # Create or update data if necessary
    
    if (is.null(normalized.data)) {
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
    
    
    
    
    # if(length(intersect(av.drugs,input$getClassifications)) > 0)
    #  CHO <- ResistanceClassifier(normalized.data, drugs=drugs, cut = cut)
    
    #     results <<- base::as.data.frame(results, row.names = NULL)
    #     rownames(results) <<- NULL
    #     return(NULL)
    #     
    
    
    CHO.single <<- ResistanceClassifierR()
    
    
    results$CycProb  <<- CHO.single$prob[,"Cyclophosphamide"]
    results$CycClass <<- CHO.single$class[,"Cyclophosphamide"]
    if ("Cyclophosphamide" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "CycProb", "CycClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("CycProb", "CycClass")) 
    }
    
    results$DoxProb  <<- CHO.single$prob[,"Doxorubicin"]
    results$DoxClass <<- CHO.single$class[,"Doxorubicin"]
    if ("Doxorubicin" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "DoxProb", "DoxClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("DoxProb", "DoxClass"))
    }
    
    results$VinProb  <<- CHO.single$prob[,"Vincristine"]
    results$VinClass <<- CHO.single$class[,"Vincristine"]
    if ("Vincristine" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "VinProb", "VinProb")
    } else {
      chosen.names <<- setdiff(chosen.names, c("VinProb", "VinProb")) 
    }
    
    results$CombProb  <<- CHO.single$prob[,"Combined"]
    results$CombClass <<- CHO.single$class[,"Combined"]
    
    if ("Combined" %in% input$getClassifications) {
      chosen.names <<- c(chosen.names, "CombProb", "CombClass")
    } else {
      chosen.names <<- setdiff(chosen.names, c("CombProb", "CombClass"))
    }
    
    if (!length(intersect(av.drugs,input$getClassifications)) > 0) {
      chosen.names <<- setdiff(chosen.names, c("CycProb", "CycClass", "DoxProb", "DoxClass",
                                               "VinProb", "VinProb","CombProb", "CombClass"))
    }
    
    results <<- base::as.data.frame(results, row.names = NULL)
    rownames(results) <<- NULL
  })
  
  
  
  observe({  
    
    classifers.ch <- input$getClassifications
    if(length(classifers.ch) > 0){
      l<- list("ABCGCB" = "ABCGCB", 
               "ABCGCB2" = "ABCGCB2",
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
  
  
  ##########################################
  ##
  ## Patient summaries
  ##
  ##########################################
  
  
  observe({ 
    
    classify()
    
    rownames(results) <- results$files
    
    metadata.in.use <- MetaDataInUse()
    
    
    
    prog.list <- NULL
    if(all(rownames(results) == rownames(metadata.in.use)) && !is.null(input$patientSummarySelectW)){
      prog.list <- list()
      out.list <- list()
      
      if(input$patientSummaryIPIW != "Choose" && input$patientSummaryIPIW %in% colnames(metadata.in.use)){
        metadata.in.use$IPI <- metadata.in.use[, input$patientSummaryIPIW]   
      }else{
        metadata.in.use$IPI <- NA
      }
      
      for(patient in input$patientSummarySelectW){
        
        if(is.na(metadata.in.use[patient, "IPI"])){
          text2 <- paste0(" and the patient have an unknown IPI score.")
        }else{
          text2 <- paste0(" and the patient have an IPI score of ", metadata.in.use[patient, "IPI"], ".")
        }
        
        prog.list[[patient]] <- paste(paste("Some Text about", patient),
                                      paste0("The cancer is of the ", results[patient, "ABCGCB2"], " type,",
                                             text2),
                                      sep="<br/>")
      } 
      
      
      for(i in 1:length(input$patientSummarySelectW)){
        showshinyalert(session, paste(input$patientSummarySelectW[i]),  
                       HTML(prog.list[[input$patientSummarySelectW[i]]]))
        
      }
      
      if(!exists(x = "PatientSummaryOpen")){
        PatientSummaryOpen <<- input$patientSummarySelectW
      }else{
        PatientSummaryOpen <<- unique(c(PatientSummaryOpen, input$patientSummarySelectW))
      }
      
      print(setdiff(PatientSummaryOpen, input$patientSummarySelectW))
      if(length(setdiff(PatientSummaryOpen, input$patientSummarySelectW)) > 0)
        for(patient in setdiff(PatientSummaryOpen, input$patientSummarySelectW))
          hideshinyalert(session, patient)
      PatientSummaryOpen <<- input$patientSummarySelectW
    }
  })
  
  output$patientSummaries <-  renderUI({ 
    classify()
    out.list <- list()
    for(i in 1:nrow(results))
      out.list[[i]] <- shinyalert(paste(results$files[i]), click.hide = TRUE)
    out.list
  })
  
  #   output$patientSumCols <-  renderUI({   
  #     out.list <- list()
  #     for(i in 1:length(input$patientSummarySelectW))
  #       out.list[[i]] <- div(class = "well container-fluid", 
  #                            div(class = "row-fluid", 
  #                                div(class = "row-fluid", 
  #                                    div(helpText(paste(input$patientSummarySelectW[i]))),
  #                                    div(class = "span3", jscolorInput(paste0("jscolorInput", input$patientSummarySelectW[i])))
  #                                )
  #                            )
  #       )
  #     out.list
  #   })
  
  observe({
    
    input$SelectColourPS
    
   
      
      
    output$SelectedColoursPS <- renderUI({
      
      isolate({
        print(input$jscolorInputPS)
        if(is.null(input$jscolorInputPS)){
          list(
            select2Input(inputId = "SelectedColoursPSw", 
                         label = "The selected colours", 
                         selected = "333333")
          )
        }else{
          if(is.null(input$jscolorInputPS)){
            new.col <- "333333"
          }else{
            if(input$jscolorInputPS == ""){
              new.col <- "333333"
            }else{
              new.col <- ifelse(input$jscolorInputPS == "FFFFFF", "333333", input$jscolorInputPS) 
            }
          }
          selected.colors <- c(input$SelectedColoursPSw, new.col)
          
          print(selected.colors)
          list(
            select2Input(inputId = "SelectedColoursPSw", 
                         label = "The selected colours", 
                         selected = selected.colors)
          )
        }
      })
    })   
  })
  
  prognosisR <- reactive({
    input$patientSummaryIPIW
    input$patientSummarySelectW
    isolate({
      
      classify()
      
      rownames(results) <- results$files
      
      
      metadata.in.use <- MetaDataInUse()
      
      
      
      prog.surv <- NULL
      if(length(rownames(results)) == length(rownames(metadata.in.use)) && 
           all(rownames(results) == rownames(metadata.in.use))){
        
        prog.surv <- list()
        
        pred.data <- as.data.frame(cbind(metadata.in.use, results))      
        
        fit.OS <<- 
          readRDS(file = "../Database/V1/Classification//fit.ABCGCB2.OS.rds")
        fit.PFS <<- 
          readRDS(file = "../Database/V1/Classification//fit.ABCGCB2.PFS.rds")
        
        metadataCombined <<-
          readRDS(file = "../Database/V1/Classification//metadataCombined.rds")
        
        if(input$patientSummaryIPIW != "Choose"){
          pred.data$IPI <- pred.data[, input$patientSummaryIPIW]   
        }else{
          pred.data$IPI <- NA
        }
        
        pred.data$IPI <- as.character(pred.data$IPI)
        
        pred.data$IPI[is.na(pred.data$IPI)] <- "NC"
        
        print(pred.data[input$patientSummarySelectW, , drop = FALSE])
        
        prog.surv[["Survfit.PFS"]] <- 
          survfit(fit.PFS, newdata = pred.data[input$patientSummarySelectW, , drop = FALSE], 
                  censor = FALSE)
        prog.surv[["Survfit.OS"]]  <- 
          survfit(fit.OS,  newdata = pred.data[input$patientSummarySelectW, , drop = FALSE], 
                  censor = FALSE)
        
        print(prog.surv)
      }
      return(prog.surv)
      
    })
  })
  
  output$patientSummaryPlot <- renderPlot({
    
    prog.surv <- prognosisR()
    
    if(is.null(prog.surv))
      return(NULL)
    
    par(mfrow = c(1, 2))
    
    if(is.null(input$SelectedColoursPSw))
      return(NULL)
    
    
    plot(prog.surv[["Survfit.OS"]],
         xlab = "Years", ylab="Survival", main = "Overall survival", col = paste0("#", input$SelectedColoursPSw))
   
    legend("bottomleft", fill = rep(paste0("#", input$SelectedColoursPSw), 50), legend = input$patientSummarySelectW)
    
    plot(prog.surv[["Survfit.PFS"]],
         xlab = "Years", ylab="Survival", main = "Progression free survival", col = paste0("#", input$SelectedColoursPSw))
    
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
  
  
  output$patientSummarySelect <- renderUI({
    
    classify()
    
    list(
      select2Input(inputId = "patientSummarySelectW", "Column containing IPI", 
                   results$files, selected =  results$files[1] )
    )    
  })
  
  
  output$patientSummaryIPI <- renderUI({
    
    metadata.in.use <- MetaDataInUse()
    
    select = NULL
    
    
    
    if(any(colnames(metadata.in.use) == "ipi"))
      select = "ipi"
    
    if(any(colnames(metadata.in.use) == "IPI"))
      select = "IPI"
    
    list(
      selectInput(inputId = "patientSummaryIPIW", "Column containing IPI", 
                  c("Choose", colnames(metadata.in.use)), selected = select)
    )    
  })
  
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
    
    
    if (length(chosen.names) == 0)
      return(NULL)
    
    
    results2 <- results[, c("files", chosen.names)]
    if(length(names(results2)) >0){
      for(i in names(results2)){
        if(class(results2[,i]) == "numeric")
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
