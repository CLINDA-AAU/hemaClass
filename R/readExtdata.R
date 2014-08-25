#' Read U133PlusVsHuEx_XXX.rds files from extdata
#' 
#' Functions to read internal data files
#' @rdname readExtdata
#' @aliases 
#'   readExtdata 
#'   readU133PlusVsHuEx_BestMatch 
#'   readU133PlusVsHuEx_GoodMatch
#'   readU133PlusVsHuEx_Complex
#' @return A data.frame of the read object
#' @examples
#' head(readU133PlusVsHuEx_BestMatch())
#' @export
readU133PlusVsHuEx_BestMatch <- function() {
  file <- system.file("extdata/U133PlusVsHuEx_BestMatch.rda", package = "MATT")
  return(readRDS(file))
}

#' @rdname readExtdata
#' @examples
#' head(readU133PlusVsHuEx_GoodMatch())
#' @export
readU133PlusVsHuEx_GoodMatch <- function() {
  file <- system.file("extdata/U133PlusVsHuEx_GoodMatch.rda", package = "MATT")
  return(readRDS(file))
}

#' @rdname readExtdata
#' @examples
#' head(readU133PlusVsHuEx_Complex())
#' @export
readU133PlusVsHuEx_Complex <- function() {
  file <- system.file("extdata/U133PlusVsHuEx_Complex.rda", package = "MATT")
  return(readRDS(file))
}


