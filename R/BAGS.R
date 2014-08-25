#' Classification of cell of origin
#' 
#' B-cell associated gene signatures (BAGS).
#' @rdname BAGS
#' @aliases 
#'   BAGS
#'   BAGSClassifier
#'   BAGSclassifier 
#' @param new.data An expression matrix.
#' @param cut.spec A numeric value for which the probabilities should be cut.
#' @param percent.classified A numeric value indicating the percentage of patients the should be classified.
#' @return Probabilities regarding each patients association with each class.
#' @details The function BAGS classifies DLBCL patients according to the cell of origin for the tumor.
#' @references Reference to the BAGS and hemaClass paper.
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' require(affy)
#' require(MATT)
#' u133 <- exprs(readRDS(system.file("extdata/GEPhgu.rda", package = "MATT")))
#' 
#' u133 <- microarrayScale(u133)
#' 
#' BAGS(u133)
#' 
#' 
#' huex <- exprs(readRDS(system.file("extdata/GEPexon.full.rda", 
#'                                   package = "MATT")))
#'
#' u133.conv  <- huex10st_to_u133(huex, method = "weighted", type = "Complex", min.pct = 0.8)
#' BAGS(u133.conv)  
#'                                                                                                                       
#' @export
BAGS <- function(new.data, cut.spec = NULL,
                 percent.classified = 85){
  
  
  mean1 <- mean(new.data, na.rm = TRUE)
  
  new.data[is.na(new.data)] <- mean1
  
  train.mat <- BAGSProbFun(new.data)
  
  if(nrow(new.data) > 1){
    class <- colnames(train.mat)[apply(train.mat, 1, which.max)]
  }else{
    class <- names(which.max(train.mat))
  }
  
  if(nrow(new.data) > 1){
    prob = apply(train.mat, 1, max)
    if(is.null(cut.spec))
      cut <- quantile(prob, 1-percent.classified / 100)    
  }else{
    cut <- 0
    prob = max(train.mat)
  }
  
  if(!is.null(cut.spec)) cut <- cut.spec
  
  class.pred <- class
  
  class <- factor(ifelse(prob < cut, "Unclassified", class),
                  levels = c("Naive", "Centroblast", "Centrocyte", 
                             "Memory", "Plasmablast", "Unclassified"))
  
  return(list(class = class, prob.mat = train.mat, 
              prob  = prob,
              cut   = cut)
  )
}




BAGSProbFun <- 
  function(newx){
    BAGS.coef <- readBAGSCoef()
    
    
    diff <- setdiff(row.names(BAGS.coef)[-1], rownames(newx))
    
    if(length(diff)){
      missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                         dimnames = list(diff, colnames(newx) ))
      
      newx <- rbind(newx, missing)
      
      warning("the following probesets are missing:\n", paste(diff, collapse = ", "))
    }
    
    x <- rbind(1, newx[row.names(BAGS.coef)[-1],,drop= FALSE])
    
    prob.mat <- matrix(ncol = 5, nrow = ncol(x))
    
    colnames(prob.mat) <- c("Naive", "Centroblast", "Centrocyte", 
                            "Memory", "Plasmablast")
    
    rownames(prob.mat) <- colnames(x)
    prob.mat[,1] <- t(x)%*%(as.matrix(BAGS.coef)[,"Naive"]) 
    prob.mat[,2] <- t(x)%*%(as.matrix(BAGS.coef)[,"Centroblast"]) 
    prob.mat[,3] <- t(x)%*%(as.matrix(BAGS.coef)[,"Centrocyte"]) 
    prob.mat[,4] <- t(x)%*%(as.matrix(BAGS.coef)[,"Memory"]) 
    prob.mat[,5] <- t(x)%*%(as.matrix(BAGS.coef)[,"Plasmablast"]) 
    
    prob.mat <- exp(prob.mat)
    prob.mat/apply(prob.mat, 1, sum)
  }