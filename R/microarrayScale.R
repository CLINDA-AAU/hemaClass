#' Scaling of expression values
#' 
#' Scale expression values prior to use of classification algorithms.
#' 
#' @param x matrix containing expression values. May also be of class ExpressionSet. 
#' @return Expression matrix consisting of genewise median centred and scaled genes.
#' @details First each probe-set is median centred. Next, each probe-set is scaled to have unit-variance.
#' @references Reference to the hemaClass.com paper
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' 2+2
#' @import matrixStats
#' @export
microarrayScale <- function(x){
  if(class(x) == "ExpressionSet"){
    x.m <- exprs(x)
  }else{
    x.m <- x
  }
  
  rowMedians <- rowMedians(x.m,  na.rm = TRUE)
  rowSd      <- matrixStats::rowSds(x.m, na.rm = TRUE)
  
  x.m <-  x.m - rowMedians
  
  x.m <- x.m / rowSd 
  
  if(class(x) == "ExpressionSet"){
    exprs(x)  <- x.m
  }else{
    x <- x.m
  }
    
  return(x)
}
