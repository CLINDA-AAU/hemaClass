#' Read coefficients for the BAGS classifier
#' 
#' Function to read coefficients for the BAGS classifier.
#' @rdname readBAGSCoef
#' @return A matrix with coefficients
#' @examples
#' head(readBAGSCoef())
#' @export
readBAGSCoef <- function() {
  file <- system.file("extdata/BAGS/BAGS.coef.rds", package = "hemaClass")
  return(readRDS(file))
}
