#' REGS for prediction of chemoresistance
#' 
#' Resistance Gene Signatures (REGS) for prediction of resistance to various drugs.
#' @rdname REGS
#' @aliases 
#'   REGS
#'   ResistanceClassifier 
#'   ResistancePredictor
#'   CyclophosphamideClassifier
#'   CyclophosphamidePredictor
#'   DoxorubicinClassifier
#'   DoxorubicinPredictor
#'   VincristineClassifier
#'   VincristinePredictor
#'   RituximabClassifier
#'   RituximabPredictor
#' @param new.data An expression matrix.
#' @param drugs An RMA reference object created by rmaPreprocessing.
#' @param cut Should the cel files be tested. When set to TRUE bad cel files are automatically discarded.
#' @param type For Rituximab, What type of classifier or predictor should be used. Current choices are corrected, uncorrected, and lysis
#' @return Expression matrix consisting of normalised array.
#' @details Load cel files into a matrix.
#' @references Reference to the hemaClass.com paper.
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' require(affy)
#' require(MATT)
#' u133 <- exprs(readRDS(system.file("extdata/GEPhgu.rda", package = "MATT")))
#' 
#' u133 <- microarrayScale(u133)
#' 
#' ResistanceClassifier(u133)
#' ResistancePredictor(u133)
#' RituximabClassifier(u133, type = "lysis") 
#' RituximabClassifier(u133, type = "uncorrected") 
#' RituximabClassifier(u133, type = "corrected") 
#' 
#' huex <- exprs(readRDS(system.file("extdata/GEPexon.full.rda", 
#'                                   package = "MATT")))
#'
#' u133.conv  <- huex10st_to_u133(huex, method = "weighted", type = "Complex")
#' ResistanceClassifier(u133.conv)                       
#'                                                                                                                                                                                                                          
#' @export
ResistanceClassifier <- function(new.data, 
                                 drugs = c("Cyclophosphamide", 
                                           "Doxorubicin",
                                           "Vincristine",
                                           "Combined"),
                                 cut = list(Cyclophosphamide = c(0.455, 0.67),
                                            Doxorubicin      = c(0.1,   0.86),
                                            Vincristine      = c(0.38,  0.54),
                                            Combined         = c(0.067, 0.907))){
  
  new.data[is.na(new.data)] <- 0
  
  train.mat <- ResistanceProbFun(new.data, setdiff(drugs, "Combined"))
  
  wh <- intersect(drugs, colnames(train.mat)) 
  
  train.mat <- train.mat[, wh, drop = FALSE]
  
  if("Combined" %in% drugs & length(length(drugs) > 3)){
    train.mat <- cbind(train.mat, apply(train.mat, 1, prod) / 
                         (apply(train.mat, 1, prod) +
                            apply(1-train.mat, 1, prod)))
    colnames(train.mat) <- drugs
  }
  
  cut <- cut[drugs]
  
  class <- train.mat
  class[,] <- "Intermediate"
  
  class[train.mat < matrix(unlist(rep(data.frame(cut)[1,], each = nrow(train.mat)) ),
                           nrow = nrow(train.mat), byrow = FALSE)] <- "Resistant"
  class[train.mat > matrix(unlist(rep(data.frame(cut)[2,], each = nrow(train.mat)) ),
                           nrow = nrow(train.mat), byrow = FALSE)] <- "Sensitive"
  
  class <- as.data.frame(class)
  
  
  for(i in 1:ncol(train.mat))
    class[, i] <-
    factor(class[, i], 
           levels= c("Sensitive","Intermediate", "Resistant"))
  
  return(list(class = class, 
              prob  = train.mat,
              cut   = cut))
}


#' @rdname REGS
#' @export
ResistancePredictor <- function(new.data, 
                                 drugs = c("Cyclophosphamide", 
                                           "Doxorubicin",
                                           "Vincristine",
                                           "Combined"),
                                 cut = list(Cyclophosphamide = c(280, 340),
                                            Doxorubicin      = c(280, 320),
                                            Vincristine      = c(115,  127),
                                            Combined         = c(200, 295))){
  
  new.data[is.na(new.data)] <- 0
  
  train.mat <- ResistancePredFun(new.data, setdiff(drugs, "Combined"))
  
  wh <- intersect(drugs, colnames(train.mat)) 
  
  train.mat <- train.mat[, wh, drop = FALSE]
  
  if("Combined" %in% drugs & length(length(drugs) > 3)){
    train.mat <- cbind(train.mat, apply(train.mat, 1, prod) / 
                         (apply(train.mat, 1, prod) +
                            apply(1-train.mat, 1, prod)))
    colnames(train.mat) <- drugs
  }
  cut <- cut[drugs]
  
  class <- train.mat
  class[,] <- "Intermediate"
  
  class[train.mat < matrix(unlist(rep(data.frame(cut)[1,], each = nrow(train.mat)) ),
                           nrow = nrow(train.mat), byrow = FALSE)] <- "Resistant"
  class[train.mat > matrix(unlist(rep(data.frame(cut)[2,], each = nrow(train.mat)) ),
                           nrow = nrow(train.mat), byrow = FALSE)] <- "Sensitive"
  
  class <- as.data.frame(class)
  
  
  for(i in 1:ncol(train.mat))
    class[, i] <-
    factor(class[, i], 
           levels= c("Sensitive","Intermediate", "Resistant"))
  
  return(list(class = class, 
              pred  = train.mat,
              cut   = cut))
}


#' @rdname REGS
#' @export
CyclophosphamideClassifier <-
  function(new.data){
    
    coef <- readCyclophosphamideClasCoef()
    
    x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
    
    prob <- t(x) %*% as.matrix((coef)  )
    
    prob <- exp(prob) / ( exp(prob) + exp(-prob))
    colnames(prob) <- "Resistant"
    prob
  }

#' @rdname REGS
#' @export
CyclophosphamidePredictor <- 
  function(new.data){
    coef <- readCyclophosphamidePredCoef()
    
    x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
    
    t(x) %*% as.matrix((coef))
  }


#' @rdname REGS
#' @export
DoxorubicinClassifier <-
  function(new.data){
    
    coef <- readDoxorubicinClasCoef()
    
    x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
    
    prob <- t(x) %*% as.matrix((coef)  )
    
    prob <- exp(prob) / ( exp(prob) + exp(-prob))
    colnames(prob) <- "Resistant"
    prob
  }

#' @rdname REGS
#' @export
DoxorubicinPredictor <- 
  function(new.data){
    coef <- readDoxorubicinPredCoef()
    
    x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
    
    t(x) %*% as.matrix((coef))
  }


#' @rdname REGS
#' @export
VincristineClassifier <-
  function(new.data){
    
    coef <- readVincristineClasCoef()
    
    x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
    
    prob <- t(x) %*% as.matrix((coef)  )
    
    prob <- exp(prob) / ( exp(prob) + exp(-prob))
    colnames(prob) <- "Resistant"
    prob
  }

#' @rdname REGS
#' @export
VincristinePredictor <- 
  function(new.data){
    coef <- readVincristinePredCoef()
    
    x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
    
    t(x) %*% as.matrix((coef))
  }

ResistancePredFun <- function(newx, drugs = c("Cyclophosphamide", "Doxorubicin", "Vincristine")){
  
  coef <- readPredCoef()
  
  diff <- setdiff(row.names(coef)[-1], rownames(newx))
  
  if(length(diff)){
    missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                       dimnames = list(diff, colnames(newx) ))
    
    newx <- rbind(newx, missing)
    
    warning("the following probesets are missing:\n", paste(diff, collapse = ", "))
  }
  
  x <- rbind(1, newx[row.names(coef)[-1],,drop= FALSE])
  
  t(x) %*% coef[, drugs, drop= FALSE]
}


ResistanceProbFun <- function(newx, drugs = c("Cyclophosphamide", "Doxorubicin", "Vincristine")){
  
  coef <- readClasCoef()
  
  diff <- setdiff(row.names(coef)[-1], rownames(newx))
  
  if(length(diff)){
    missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                       dimnames = list(diff, colnames(newx) ))
    
    newx <- rbind(newx, missing)
    
    warning("the following probesets are missing:\n", paste(diff, collapse = ", "))
  }
  x <- rbind(1, newx[row.names(coef)[-1],,drop= FALSE])
  
  prob.mat <- t(x) %*% coef[, drugs, drop= FALSE]
  
  exp(-prob.mat) / ( exp(prob.mat) + exp(-prob.mat))
}



#' @rdname REGS
#' @export
RituximabClassifier <-
  function(new.data, type = "corrected", cut = c(0.33, 0.66), 
           cut.spec = NULL, percent.classified = 85){
    
    if(type == "lysis"){
      
      new.data[is.na(new.data)] <- 0
      
      train.mat <- RituximabProbFun(new.data)
      
      if(nrow(new.data) > 1){
        class <- colnames(train.mat)[apply(train.mat, 1, which.max)]
      }else{
        class <- names(which.max(train.mat))
      }
      
      if(nrow(new.data) > 1){
        prob = apply(train.mat, 1, max)
        if(is.null(cut.spec))
          cut <- quantile(prob, 1-percent.classified / 100)    
      }else{
        cut <- 0
        prob = max(train.mat)
      }
      
      if(!is.null(cut.spec)) cut <- cut.spec
      
      class.pred <- class
      
      class <- factor(ifelse(prob < cut, "Unclassified", class),
                      levels = c("Lytisk", "Statisk", "Resistant", "Unclassified"))
      
      return(list(class = class, 
                  prob = train.mat,
                  cut   = cut)
      )
        
    }else{
      
      if(type == "uncorrected")
        coef <- readRituximabClasCoef()
      if(type == "corrected")
        coef <- readRituximabClasCorCoef()
      
      x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
      
      prob <- t(x) %*% as.matrix((coef)  )
      
      prob <- exp(prob) / ( exp(prob) + exp(-prob))
      colnames(prob) <- "Resistant"
      prob
      
    }
  }

#' @rdname REGS
#' @export
RituximabPredictor <- 
  function(new.data, type = "corrected"){
    if(type == "uncorrected")
      coef <- readRituximabPredCoef()
    if(type == "corrected")
      coef <- readRituximabPredCorCoef()
    
    x <- rbind(1, new.data[names(coef)[-1],,drop= FALSE])
    
    t(x) %*% as.matrix((coef))
  }


RituximabProbFun <- function(newx){
 
  coef <- readRituximabClasLytiskCoef()
  
  x <- rbind(1, newx[row.names(coef)[-1],,drop= FALSE])
  
  prob.mat <- matrix(ncol = 3, nrow = ncol(x))
  
  colnames(prob.mat) <- c("Lytisk", "Statisk", "Resistant")
  
  rownames(prob.mat) <- colnames(x)
  prob.mat[,1] <- t(x)%*%(as.matrix(coef)[,"Lytisk"]) 
  prob.mat[,2] <- t(x)%*%(as.matrix(coef)[,"Statisk"]) 
  prob.mat[,3] <- t(x)%*%(as.matrix(coef)[,"Resistant"]) 
  
  prob.mat <- exp(prob.mat)
  prob.mat / rowSums(prob.mat)
}
