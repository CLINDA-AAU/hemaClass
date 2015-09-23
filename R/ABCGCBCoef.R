#' Read coefficients for the ABC/GCB classifier
#' 
#' Function to read or get the coefficients used in the linear predictor score 
#' of the ABC/GCB classifier.
#' 
#' @rdname readABCGCBCoef
#' @return A \code{matrix} with the coefficients.
#' @examples
#' coefs <- readABCGCBCoef()
#' head(coefs)
#' str(coefs)
#' @export
readABCGCBCoef <- function() {
  # Function to read the ABC/GCB coefficients
  file <- system.file("extdata/ABCGCB/ABCGCB.coef.rds", package = "hemaClass")
  return(readRDS(file))
}
