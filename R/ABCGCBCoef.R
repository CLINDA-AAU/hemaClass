#' Read coefficients for the ABC/GCB classifier
#' 
#' Function to read coefficients for the ABC/GCB classifier.
#' @rdname readABCGCBCoef
#' @return A matrix with coefficients
#' @examples
#' head(readABCGCBCoef())
#' @export
readABCGCBCoef <- function() {
  # Function to read the ABC/GCB coefficients
  file <- system.file("extdata/ABCGCB/ABCGCB.coef.rds", package = "hemaClass")
  return(readRDS(file))
}
