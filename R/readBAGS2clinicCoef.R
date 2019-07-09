#' Read coefficients for the BAGS2clinic classifier
#' 
#' Function to read coefficients for the BAGS2clinic classifier.
#' @rdname readBAGSCoef
#' @return A matrix with coefficients
#' @examples
#' head(readBAGS2clinicCoef())
#' @export
readBAGS2clinicCoef <- function() {
  file <- system.file("extdata/BAGS2clinic/BAGS2ClinicCoef_tech.rds", package = "hemaClass")
  return(readRDS(file))
}
