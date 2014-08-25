#' Read coefficients for the ABC/GCB classifier
#' 
#' Function to read coefficients for the ABC/GCB classifier.
#' @rdname readABCGCBCoef
#' @return A matrix with coefficients
#' @examples
#' head(readABCGCBCoef())
#' @export
readABCGCBCoef <- function() {
  file <- system.file("extdata/ABCGCB/ABCGCB.coef.rda", package = "hemaClass")
  return(readRDS(file))
}
