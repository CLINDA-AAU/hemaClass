#' Classification of cell of origin
#' 
#' Classification according to ABC/GCB.
#' @rdname ABCGCB
#' @aliases 
#'   ABCGCB
#'   ABCGCBClassifier
#'   ABCGCBclassifier 
#' @param new.data An expression matrix.
#' @param NC.range A numeric vector with values for which the probabilities should be cut.
#' @return Probabilities regarding each patients association with each class.
#' @details The function ABC/GCB classifies DLBCL patients according to the cell of origin for the tumor.
#' @references Reference to the ABC/GCB and hemaClass paper.
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' require(affy)
#' require(MATT)
#' u133 <- exprs(readRDS(system.file("extdata/GEPhgu.rda", package = "MATT")))
#' 
#' u133 <- microarrayScale(u133)
#' 
#' ABCGCB(u133)
#' 
#' 
#' huex <- exprs(readRDS(system.file("extdata/GEPexon.full.rda", 
#'                                   package = "MATT")))
#'
#' u133.conv  <- huex10st_to_u133(huex, method = "weighted", type = "Complex", min.pct = 0.95)
#' ABCGCB(u133.conv)  
#'                                                                                                                       
#' @export
ABCGCB <- function(new.data, NC.range = c(0.1, 0.9)) {
  
  # mean1 <- mean(new.data, na.rm = TRUE)
  
  new.data[is.na(new.data)] <- 0 #<- mean1
  
  prob <- ABCGCBProbFun(new.data) 
  
  class <- rep("NC", length(prob))
  class[prob < NC.range[1]] <- "GCB"
  class[prob > NC.range[2]] <- "ABC"
  
  class <- factor(class, levels=c("ABC", "NC", "GCB"))
  return(list(prob = prob, class = class))
}


ABCGCBProbFun <- function(newx){
  
  ABCGCB.coef <- readABCGCBCoef()
  
  diff <- setdiff(row.names(ABCGCB.coef)[-1], rownames(newx))
  
  if(length(diff)){
    missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                       dimnames = list(diff, colnames(newx) ))
    
    newx <- rbind(newx, missing)
    
    warning("the following probesets are missing:\n", paste(diff, collapse = ", "))
  }
  
  x <- rbind(1, newx[row.names(ABCGCB.coef)[-1],,drop= FALSE])
  
  prob.mat <- matrix(ncol = 1, nrow = ncol(x))
  
  rownames(prob.mat) <- colnames(x)
  prob.mat[,1] <- t(x)%*%ABCGCB.coef
  
  1 / ( 1 + exp(-prob.mat))
}