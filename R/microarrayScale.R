#' Scaling of expression values
#' 
#' Scale expression values prior to use of classification algorithms.
#' 
#' @param x matrix containing expression values. May also be of class 
#'   \code{ExpressionSet}. 
#' @param center How should the data be centered. Can be either \code{"median"}, 
#'   \code{"mean"}, or \code{"FALSE"}.
#' @param scale How should the data be scaled. Can be either \code{"sd"} or 
#'   \code{"FALSE"}.
#' @return Expression matrix consisting of genewise median centred and scaled 
#'   genes.
#' @details First each probe-set is median centred. Next, each probe-set is 
#'   scaled to have unit-variance.
#' @references Reference to the \code{hemaClass.com} paper
#' @author 
#'   Steffen Falgreen <sfl (at) rn.dk> \cr
#'   Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' 2+2
#' @importMethodsFrom Biobase exprs "exprs<-"
#' @importFrom matrixStats rowMedians
#' @export
microarrayScale <- function(x, center = "median", scale = "sd") {
  if (class(x) == "ExpressionSet") {
    x.m <- Biobase::exprs(x)
  } else {
    x.m <- x
  }
  
  if(center == "median") {
    x.m <-  x.m - matrixStats::rowMedians(x.m,  na.rm = TRUE)
  }
  if(center == "mean") {
    x.m <-  x.m - rowMeans(x.m,  na.rm = TRUE)
  }
  if (scale == "sd") {
    x.m <- x.m / rowSds(x.m, na.rm = TRUE) 
  }
  if (class(x) == "ExpressionSet") {
    Biobase::exprs(x)  <- x.m
  } else {
    x <- x.m
  }
  
  return(x)
}
