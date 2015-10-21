#' Fit overall and progression free survival
#' 
#' Internal functions to fit overall survival and progression free survival 
#' from IDRC, LLMPP, and MDFCI data.
#'
#' @rdname fitOS
#' @aliases fitOS fitPFS
#' @return An object of class \code{coxph}, see \code{\link[survival]{coxph}}.
#' @examples
#' fitOS()
#' @importFrom survival coxph
#' @keywords internal
fitOS <- function() {
  file <- system.file("website/Database/Classification/metadataCombined.rds", 
                      package = "hemaClass")
  metadataCombined <- readRDS(file)
  fit <- coxph(formula = OS ~ IPI + ABCGCB2, data = metadataCombined)
  return(fit)
}

#' @rdname fitOS
#' @examples
#' fitPFS()
fitPFS <- function() {
  file <- system.file("website/Database/Classification/metadataCombined.rds", 
                      package = "hemaClass")
  metadataCombined <- readRDS(file)
  fit <- coxph(formula = PFS ~ IPI + ABCGCB2, data = metadataCombined)
  return(fit)
}
