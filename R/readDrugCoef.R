#' Read coefficients for REGS classifiers and predictors
#' 
#' Functions to read coefficients for REGS classifiers and predictors.
#' @rdname readDrugCoef
#' @aliases 
#'   readDrugCoef 
#'   readPredCoef
#'   readClasCoef
#'   readCyclophosphamideClasCoef 
#'   readDoxorubicinClasCoef
#'   readVincristineClasCoef
#'   readCyclophosphamidePredCoef 
#'   readDoxorubicinPredCoef
#'   readVincristinePredCoef
#' @return A matrix with coefficients
#' @examples
#' head(readPredCoef())
#' @export
readPredCoef <- function() {
  file <- system.file("extdata/Drugs/resistance.predictor.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readClasCoef())
#' @export
readClasCoef <- function() {
  file <- system.file("extdata/Drugs/resistance.classifier.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readCyclophosphamideClasCoef())
#' @export
readCyclophosphamideClasCoef <- function() {
  file <- system.file("extdata/Drugs/Cyclophosphamide.resistance.class.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readCyclophosphamidePredCoef())
#' @export
readCyclophosphamidePredCoef <- function() {
  file <- system.file("extdata/Drugs/Cyclophosphamide.resistance.pred.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readDoxorubicinClasCoef())
#' @export
readDoxorubicinClasCoef <- function() {
  file <- system.file("extdata/Drugs/Doxorubicin.resistance.class.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readDoxorubicinPredCoef())
#' @export
readDoxorubicinPredCoef <- function() {
  file <- system.file("extdata/Drugs/Doxorubicin.resistance.pred.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readVincristineClasCoef())
#' @export
readVincristineClasCoef <- function() {
  file <- system.file("extdata/Drugs/Vincristine.resistance.class.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readVincristinePredCoef())
#' @export
readVincristinePredCoef <- function() {
  file <- system.file("extdata/Drugs/Vincristine.resistance.pred.coef.rda", package = "hemaClass")
  return(readRDS(file))
}


#' @rdname readDrugCoef
#' @examples
#' head(readRituximabClasCoef())
#' @export
readRituximabClasCoef <- function() {
  file <- system.file("extdata/Drugs/Rituximab.resistance.class.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readRituximabPredCoef())
#' @export
readRituximabPredCoef <- function() {
  file <- system.file("extdata/Drugs/Rituximab.resistance.pred.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readRituximabClasCorCoef())
#' @export
readRituximabClasCorCoef <- function() {
  file <- system.file("extdata/Drugs/Rituximab.resistance.class.Cor.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readRituximabPredCorCoef())
#' @export
readRituximabPredCorCoef <- function() {
  file <- system.file("extdata/Drugs/Rituximab.resistance.pred.Cor.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readRituximabClasLytiskCoef())
#' @export
readRituximabClasLytiskCoef <- function() {
  file <- system.file("extdata/Drugs/Rituximab.resistance.class.Lytisk.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

readRituximabClasLytisk2Coef <- function() {
  file <- system.file("extdata/Drugs/Rituximab.resistance.class.Lytisk2.coef.rda", package = "hemaClass")
  return(readRDS(file))
}


#' @rdname readDrugCoef
#' @examples
#' head(readDexamethasoneClasCoef())
#' @export
readDexamethasoneClasCoef <- function() {
  file <- system.file("extdata/Drugs/Dexamethasone.resistance.class.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readDexamethasonePredCoef())
#' @export
readDexamethasonePredCoef <- function() {
  file <- system.file("extdata/Drugs/Dexamethasone.resistance.pred.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readMelphalanClasCoef())
#' @export
readMelphalanClasCoef <- function() {
  file <- system.file("extdata/Drugs/Melphalan.resistance.class.coef.rda", package = "hemaClass")
  return(readRDS(file))
}

#' @rdname readDrugCoef
#' @examples
#' head(readMelphalanPredCoef())
#' @export
readMelphalanPredCoef <- function() {
  file <- system.file("extdata/Drugs/Melphalan.resistance.pred.coef.rda", package = "hemaClass")
  return(readRDS(file))
}


