#' Fit progression free survival
#' 
#' Function to fit progression free survival.
#' @rdname fitPFS
#' @return An object of type coxph
#' @examples
#' fitPFS()
#' @import survival
#' @export

fitPFS <- function() {
  path <- system.file("website/Database/Classification/", package = "hemaClass")
  metadataCombined <- readRDS(paste0(path, "metadataCombined.rds"))
  
  fit <- coxph(formula = PFS ~ IPI + ABCGCB2, data = metadataCombined)
  return(fit)
}