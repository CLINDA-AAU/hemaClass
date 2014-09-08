#' RMA pre-process cel files read into R using readCelfiles according to a reference.
#' 
#' Pre-process cel files according to the RMA method according to a reference build using rmaPreprocessing.
#' 
#' @param affy.batch An object created by readCelfiles.
#' @param reference An RMA reference object created by rmaPreprocessing.
#' @param test Should the cel files be tested. When set to TRUE bad cel files are automatically discarded.
#' @return Expression matrix consisting of normalised array.
#' @details Load cel files into a matrix.
#' @references Reference to the hemaClass.com paper.
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' files <- dir(system.file("extdata/celfiles", package = "hemaClass"), full.names = TRUE)
#' affy.batch <- readCelfiles(files)
#' 
#' ref.affy   <- rmaPreprocessing(affy.batch)
#' ref.affy.2 <- rmaPreprocessing(affy.batch, quantile = ref.affy$quantile)
#' all(ref.affy.2$exprs - ref.affy$exprs < 0.00000001)
#' 
#' user.affy  <- rmaReference(affy.batch, ref.affy)
#' 
#' all(user.affy$exprs - ref.affy$exprs < 0.00000001)
#' @import preprocessCore
#' @export
rmaReference <- function(affy.batch, reference, test = FALSE){
  
  # Get the probeset information
  probesets <- affy.batch$probesets
  
  #Get the pm probes
  ref.pm   <- affy.batch$exprs
  
  # Background correction  
  ref.pm <- preprocessCore::rma.background.correct(ref.pm, copy = TRUE)
  colnames(ref.pm) <- colnames(affy.batch$exprs)
  rownames(ref.pm) <- rownames(affy.batch$exprs)
  # Test for bad arrays
  contin = TRUE
  if(test == TRUE){
    wh <- apply(!is.finite(ref.pm), 2, any)
    wh2 <- apply(is.na(ref.pm), 2, any)
    
    bad <- colnames(affy.batch$exprs)[wh|wh2]
    good <- setdiff(colnames(ref.pm), bad)
    if(length(good) > 0){
      ref.pm <- ref.pm[, good]
    }else{
      warning("None of the supplied microarrays passed the quality control")
      contin <- FALSE
    }   
    if(length(bad) > 0)
      warning("The following arrays were discarded: ", paste(bad, collapse = ", "))
  }else{
    bad <- NULL
  }
  
  # Normalisation and summarisation accoriding to reference
  if(contin){
    tmp <- userRMA(ref.pm , probesets, colnames(ref.pm), 
                   reference$quantile,  reference$alpha)
    
    # Scale the rma normalised
    tmp$exprs.sc <- (tmp$exprs -reference$median) / reference$sd
    
    return(tmp)
  }else{
    bad
  }
}

