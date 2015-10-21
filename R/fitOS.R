#' Fit overall survival
#' 
#' Function to fit overall survival.
#' @rdname fitOS
#' @return An object of type coxph
#' @examples
#' fitOS()
#' @import survival
#' @export

fitOS <- function() {
  path <- system.file("website/Database/Classification/", package = "hemaClass")
  metadataCombined <- readRDS(paste0(path, "metadataCombined.rds"))
  
  fit <- coxph(formula = OS ~ IPI + ABCGCB2, data = metadataCombined)
  return(fit)
}
