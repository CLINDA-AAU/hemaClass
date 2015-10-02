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
#'   DexamethasoneClassifier
#'   DexamethasonePredictor
#'   MelphalanClassifier
#'   MelphalanPredictor
#' @param new.data An expression matrix.
#' @param drugs An RMA reference object created by rmaPreprocessing.
#' @param cut Should the \code{.cel} files be tested. When set to \code{TRUE}  
#'   bad \code{.cel} files are automatically discarded.
#' @param type For Rituximab, What type of classifier or predictor should be
#'   used. Current choices are corrected, uncorrected, lysis, and lysis2.
#' @param calc.cut For Rituximab, calculate the cutpoints according to 
#'   proportions in the data. E.g. \code{calc.cut = c(0.33, 0.66)} means that a 
#'  third is deemed sensitive, intermediate, and resistant respectively.
#' @param cut.spec For the lysis type rituximab classifier specify the cut 
#'   point for unclassified.
#' @param percent.classified For the lysis type rituximab classifier specify 
#'   the percentage of unclassified.
#' @return Expression matrix consisting of normalised array.
#' @details Load cel files into a matrix.
#' @references Reference to the \code{hemaClass.com} paper.
#' @author 
#' Steffen Falgreen <sfl (at) rn.dk> \cr 
#' Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' \donttest{
#' files <- dir(system.file("extdata/celfiles", package = "hemaClass"), 
#'              full.names = TRUE)
#' affyBatch <- readCelfiles(filenames = files)
#' 
#' # The cel files are pre-processed
#' affyRMA <- rmaPreprocessing(affyBatch)
#' 
#' # the function rmaPreprocessing returns median centered and scaled 
#' # expression values in the slot exprs.sc. 
#' 
#' # The slot exprs.sc.mean contains mean cetered and scaled expression values.
#' # This scaling can also be achieved using the function microarrayScale.
#' affyRMA.sc <- microarrayScale(affyRMA$exprs, center = "median")
#' 
#' # We may now use the predictors
#' 
#' # The classifier for Cyclophosphamide, Doxorubicin, and Vincristine:
#' ResistanceClassifier(affyRMA.sc)
#' 
#' # The predictor for Cyclophosphamide, Doxorubicin, and Vincristine:
#' ResistancePredictor(affyRMA.sc)
#' 
#' # The classifier for Rituximab into Lysis, Statisk, or Resistant:
#' RituximabClassifier(affyRMA.sc, type = "lysis2", percent.classified = 100) 
#' 
#' # The classifier for Rituximab into Sensitive, Intermediate, or Resistant without 
#' # taking human serum into accout:
#' RituximabClassifier(affyRMA.sc, type = "uncorrected", calc.cut = c(0.33, 0.66)) 
#' 
#' # The classifier for Rituximab into Sensitive, Intermediate, or Resistant 
#' # while taking human serum into accout:
#' RituximabClassifier(affyRMA.sc, type = "corrected") 
#' 
#' # For the melphalan classifier we use mean centered and sd scaled expression values:
#' affyRMA.sc.mean <- microarrayScale(affyRMA$exprs, center = "mean")
#' MelphalanClassifier(affyRMA.sc.mean) 
#' 
#' # For the melphalan predictor we use the original scale:
#' MelphalanPredictor(affyRMA$exprs)  
#' }                                                                                                                                                                                                                                  
#' @export
ResistanceClassifier <- function(new.data, 
                                 drugs = c("Cyclophosphamide", 
                                           "Doxorubicin",
                                           "Vincristine",
                                           "Combined"),
                                 cut = list(Cyclophosphamide = c(0.455, 0.67),
                                            Doxorubicin      = c(0.1,   0.86),
                                            Vincristine      = c(0.38,  0.54),
                                            Combined         = c(0.067, 0.907))) {
  
  new.data[is.na(new.data)] <- 0
  
  train.mat <- ResistanceProbFun(new.data, setdiff(drugs, "Combined"))
  
  wh <- intersect(drugs, colnames(train.mat)) 
  
  train.mat <- train.mat[, wh, drop = FALSE]
  
  if ("Combined" %in% drugs & length(length(drugs) > 3)) {
    train.mat <- cbind(train.mat, apply(train.mat, 1, prod) / 
                         (apply(train.mat, 1, prod) +
                            apply(1 - train.mat, 1, prod)))
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
  
  
  for (i in 1:ncol(train.mat)) {
    class[, i] <- factor(class[, i], 
                         levels = c("Sensitive", "Intermediate", "Resistant"))
  }
  
  return(list(class = class, prob  = train.mat, cut   = cut))
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
  
  if ("Combined" %in% drugs & length(length(drugs) > 3)) {
    train.mat <- cbind(train.mat, apply(train.mat, 1, prod) / 
                         (apply(train.mat, 1, prod) +
                            apply(1 - train.mat, 1, prod)))
    colnames(train.mat) <- drugs
  }
  
  cut <- cut[drugs]
  
  class <- train.mat
  class[,] <- "Intermediate"
  
  # Check that these are the correct way
  class[train.mat < matrix(unlist(rep(data.frame(cut)[1,], each = nrow(train.mat))),
                           nrow = nrow(train.mat), byrow = FALSE)] <- "Sensitive"
  class[train.mat > matrix(unlist(rep(data.frame(cut)[2,], each = nrow(train.mat))),
                           nrow = nrow(train.mat), byrow = FALSE)] <- "Resistant"
  
  class <- as.data.frame(class)
  
  
  for (i in 1:ncol(train.mat)) {
    class[, i] <- factor(class[, i], 
                         levels = c("Sensitive", "Intermediate", "Resistant"))
  }
  return(list(class = class, 
              pred  = train.mat,
              cut   = cut))
}


#' @rdname REGS
#' @export
CyclophosphamideClassifier <- function(new.data) {
  coef <- readCyclophosphamideClasCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  prob <- t(x) %*% as.matrix((coef))
  
  prob <- 1 - exp(prob) / (exp(prob) + exp(-prob))
  colnames(prob) <- "Sensitive"
  return(prob)
}

#' @rdname REGS
#' @export
CyclophosphamidePredictor <- function(new.data) {
  coef <- readCyclophosphamidePredCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  return(t(x) %*% as.matrix(coef))
}


#' @rdname REGS
#' @export
DoxorubicinClassifier <- function(new.data) {
  coef <- readDoxorubicinClasCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  prob <- t(x) %*% as.matrix((coef))
  
  prob <- 1 - exp(prob) / (exp(prob) + exp(-prob))
  colnames(prob) <- "Sensitive"
  return(prob)
}

#' @rdname REGS
#' @export
DoxorubicinPredictor <- function(new.data) {
  coef <- readDoxorubicinPredCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  return(t(x) %*% as.matrix(coef))
}


#' @rdname REGS
#' @export
VincristineClassifier <- function(new.data) {
  coef <- readVincristineClasCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  prob <- t(x) %*% as.matrix((coef)  )
  
  prob <- 1 - exp(prob) / (exp(prob) + exp(-prob))
  colnames(prob) <- "Sensitive"
  return(prob)
}

#' @rdname REGS
#' @export
VincristinePredictor <- function(new.data) {
  coef <- readVincristinePredCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  return(t(x) %*% as.matrix((coef)))
}

ResistancePredFun <- function(newx, 
                              drugs = c("Cyclophosphamide", "Doxorubicin", 
                                        "Vincristine")) {
  
  coef <- readPredCoef()
  
  diff <- setdiff(row.names(coef)[-1], rownames(newx))
  
  if (length(diff)) {
    missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                       dimnames = list(diff, colnames(newx) ))
    
    newx <- rbind(newx, missing)
    
    warning("the following probesets are missing:\n", 
            paste(diff, collapse = ", "))
  }
  
  x <- rbind(1, newx[row.names(coef)[-1], , drop = FALSE])
  
  return(t(x) %*% coef[, drugs, drop = FALSE])
}


ResistanceProbFun <- function(newx, 
                              drugs = c("Cyclophosphamide", "Doxorubicin", 
                                        "Vincristine")) {
  
  coef <- readClasCoef()
  
  diff <- setdiff(row.names(coef)[-1], rownames(newx))
  
  if (length(diff)) {
    missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                       dimnames = list(diff, colnames(newx) ))
    
    newx <- rbind(newx, missing)
    
    warning("the following probesets are missing:\n", 
            paste(diff, collapse = ", "))
  }
  x <- rbind(1, newx[row.names(coef)[-1], , drop = FALSE])
  
  prob.mat <- t(x) %*% coef[, drugs, drop = FALSE]
  
  return(exp(-prob.mat) / (exp(prob.mat) + exp(-prob.mat)))
}



#' @rdname REGS
#' @export
RituximabClassifier <- function(new.data,
                                type = "corrected",
                                cut = c(0.33, 0.66), 
                                calc.cut = NULL,
                                cut.spec = NULL, 
                                percent.classified = 85){
  
  if (grepl("lysis", type)) {
    
    new.data[is.na(new.data)] <- 0
    
    train.mat <- RituximabProbFun(new.data, type = type)
    
    if (nrow(new.data) > 1) {
      class <- colnames(train.mat)[apply(train.mat, 1, which.max)]
    } else {
      class <- names(which.max(train.mat))
    }
    
    if (nrow(new.data) > 1) {
      prob = apply(train.mat, 1, max)
      if (is.null(cut.spec)) {
        cut <- quantile(prob, 1 - percent.classified / 100)    
      }
    } else {
      cut <- 0
      prob = max(train.mat)
    }
    
    if (!is.null(cut.spec)) {
      cut <- cut.spec
    }
    
    class <- 
      factor(ifelse(prob < cut, "Unclassified", class),
             levels = c("Lytisk", "Statisk", "Resistant", "Unclassified"))
    class <- as.matrix(class)
    colnames(class) <- "Sensitivity"
    
    return(list(class = class, prob = train.mat, cut = cut))
    
  } else {
    
    if (type == "uncorrected") {
      coef <- readRituximabClasCoef()
    }
    if (type == "corrected") {
      coef <- readRituximabClasCorCoef()
    }
    
    x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
    
    prob <- t(x) %*% as.matrix(coef)
    
    prob <- exp(prob) / (exp(prob) + exp(-prob))
    colnames(prob) <- "Resistant"
    
    prob <- 1 - prob # to obtain probability of being sensitive
    colnames(prob) <- "Sensitivity"
    
    if (!is.null(calc.cut)) {
      cut <- quantile(prob, calc.cut)
    }
    class <- prob
    class[] <- "Intermediate"
    class[prob < min(cut)] <- "Resistant"
    class[prob > max(cut)] <- "Sensitive"
    
    class <- as.data.frame(class)
    
    class[,1] <- factor(class[,1], 
                        levels = c("Sensitive", "Intermediate", "Resistant"))
    
    return(list(class = class, prob = prob, cut = cut))
  }
}

#' @rdname REGS
#' @export
RituximabPredictor <- function(new.data, type = "corrected", 
                               cut = c(0.33, 0.66), calc.cut = NULL) {
  
  if (type == "uncorrected") {
    coef <- readRituximabPredCoef()
  } else if (type == "corrected") {
    coef <- readRituximabPredCorCoef()
  }
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  prob <- t(x) %*% as.matrix((coef))
  
  # calculate classes
  class <- prob
  
  if (!is.null(calc.cut)) {
    cut <- quantile(prob, calc.cut)
  }
  class[] <- "Intermediate"
  
  if (type == "uncorrected") {
    class[prob < min(cut)] <- "Sensitive"
    class[prob > max(cut)] <- "Resistant"
  }
  
  if (type == "corrected") {
    class[prob < min(cut)] <- "Resistant"
    class[prob > max(cut)] <- "Sensitive"
  }
  
  class <- as.data.frame(class)
  
  class[,1] <- factor(class[,1], 
                      levels = c("Sensitive", "Intermediate", "Resistant"))
  
  return(list(class = class, 
              pred  = prob,
              cut   = cut))
  
}


RituximabProbFun <- function(newx, type = "lysis") {
  if (type == "lysis") {
    coef <- readRituximabClasLytiskCoef()
  } else if (type == "lysis2") {
    coef <- readRituximabClasLytisk2Coef()
  }
  
  x <- rbind(1, newx[row.names(coef)[-1], , drop = FALSE])
  
  prob.mat <- matrix(ncol = 3, nrow = ncol(x))
  
  colnames(prob.mat) <- c("Lytisk", "Statisk", "Resistant")
  
  rownames(prob.mat) <- colnames(x)
  prob.mat[,1] <- t(x) %*% as.matrix(coef)[, "Lytisk"] 
  prob.mat[,2] <- t(x) %*% as.matrix(coef)[, "Statisk"]
  prob.mat[,3] <- t(x) %*% as.matrix(coef)[, "Resistant"]
  
  prob.mat <- exp(prob.mat)
  return(prob.mat / rowSums(prob.mat))
}



#' @rdname REGS
#' @export
DexamethasoneClassifier <- function(new.data){
  warning("The Dexamethasone classifier is still experimental! ",
          "Use with caution.")
  coef <- readDexamethasoneClasCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  prob <- t(x) %*% as.matrix(coef)
  prob <- 1 - exp(prob)/(exp(prob) + exp(-prob))
  colnames(prob) <- "Sensitive"
  return(prob)
}

#' @rdname REGS
#' @export
DexamethasonePredictor <- function(new.data) {
  warning("The Dexamethasone classifier is still experimental! ",
          "Use with caution.")
  coef <- readDexamethasonePredCoef()
  
  x <- rbind(1, new.data[names(coef)[-1], , drop = FALSE])
  
  return(t(x) %*% as.matrix((coef)))
}

#' @rdname REGS
#' @export
MelphalanClassifier <- function(new.data) {
  coef <- readMelphalanClasCoef()
  
  x <- rbind(1, as.matrix(new.data)[colnames(coef)[-1], , drop = FALSE])
  
  probs <- t(tcrossprod(coef, t(x)))
  probs <- exp(probs - probs[cbind(1:nrow(probs), 
                                   max.col(probs, ties.method = "first"))])
  probs <- zapsmall(probs/rowSums(probs))
  class <- factor(rownames(coef)[max.col(probs)], 
                  levels = c("Sensitive", "Intermediate", "Resistant"))
  
  return(list(probs = probs, class = class))
}

#' @rdname REGS
#' @export
MelphalanPredictor <- function(new.data) {
  fit <- readMelphalanPredCoef()
  
  newx <- t(as.matrix(new.data)[rownames(fit$beta)[-1], , drop = FALSE])
  newx <- scale(newx, fit$center, fit$scale)
  
  return(cbind(1, newx) %*% fit$beta)
}


