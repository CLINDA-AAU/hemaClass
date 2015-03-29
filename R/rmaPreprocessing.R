#' RMA pre-process \code{.CEL} files read into R using readCelfiles
#' 
#' Pre-process \code{.CEL} files according to the RMA method and create a 
#' reference for later use.
#' 
#' @param affy.batch An object created by \code{readCelfiles}.
#' @param test Should the \code{.CEL} files be tested. When set to \code{TRUE}  
#'   bad \code{.CEL} files are automatically discarded.
#' @param quantile Specify a vector of quantiles that the data should be 
#'   normalized to.
#' @return Expression matrix consisting of normalised array.
#' @details Load \code{.CEL} files into a matrix.
#' @references 
#'   \code{hemaClass.com}
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr 
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' files <- dir(system.file("extdata/celfiles", package = "hemaClass"), 
#'              full.names = TRUE)
#' affyBatch <- readCelfiles(filenames = files[1])
#' 
#' affyRMA <- rmaPreprocessing(affyBatch)
#' @import preprocessCore
#' @importFrom matrixStats rowMedians
#' @export
rmaPreprocessing <- function(affy.batch, test = FALSE, quantile = NULL){
  
  # Get the probeset information
  probesets <- affy.batch$probesets
  
  #Get the pm probes
  ref.pm   <- affy.batch$exprs
  
  # background correction  
  ref.pm <- preprocessCore::rma.background.correct(ref.pm, copy = TRUE)
  colnames(ref.pm) <- colnames(affy.batch$exprs)
  rownames(ref.pm) <- rownames(affy.batch$exprs)
  
  contin = TRUE
  if(test == TRUE){
    wh  <- colSums(!is.finite(ref.pm)) > 0 #apply(!is.finite(ref.pm), 2, any)
    wh2 <- colSums(is.na(ref.pm)) > 0 # apply(is.na(ref.pm), 2, any)
    
    bad  <- colnames(affy.batch$exprs)[wh|wh2]
    good <- setdiff(colnames(affy.batch$exprs), bad)
    if(length(good) > 0){
      ref.pm <- ref.pm[, good]
    }else{
      warning("None of the supplied microarrays passed the quality control")
      contin <- FALSE
    }
    if(length(bad) > 0)
      warning("The following arrays were discarded: ", paste(bad,collapse=", "))
  }else{
    bad <- NULL
  }
  if(contin){
    # quantile normalisation
    if(is.null(quantile)){
      generateQuan <- 1
      quantile <- rep(0, nrow(ref.pm))
    }else{
      generateQuan = 0
    }
      
    affy.batch <- RMA_norm(ref.pm, quantile, generateQuan)
    
    # log2 transformation of the data
    ref.pm.log <- log2(affy.batch$exprs)
    
    # sort the data according the probesets
    #ref.pm.log <- ref.pm.log[paste(unlist(probesets)),]
    
    # Use the Rcpp based median polish to estimate exprs levels and 
    tmp <- RMA_sum(ref.pm.log, 
                   probesets, rownames(ref.pm.log), 
                   colnames(ref.pm.log))
    
    # estimate median for later centralisation
    ref.median <- matrixStats::rowMedians(as.matrix(tmp$exprs))
    names(ref.median) <- rownames(tmp$exprs)
    
    # estimate standard deviations for later normalisation
    ref.sd <- rowSds(as.matrix(tmp$exprs))
    
    ref.mean <- rowMeans(as.matrix(tmp$exprs))
      
    exprs.sc <- (tmp$exprs - ref.median) / ref.sd
    
    exprs.sc.mean <- (tmp$exprs - ref.mean) / ref.sd
   
    return(list(exprs = tmp$exprs, exprs.sc = exprs.sc, exprs.sc.mean = exprs.sc.mean, 
                quantile = affy.batch$quantile, alpha = tmp$alpha, 
                sd = ref.sd, median = ref.median, mean = ref.mean, bad = bad))
  } else {
    return(bad)
  }
}

