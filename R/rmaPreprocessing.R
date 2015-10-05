#' RMA pre-process \code{.CEL} files 
#' 
#' Pre-process \code{.CEL} files read into \R using 
#' \code{\link{readCelfiles}} according to the regular cohort RMA method and 
#' create a reference for later use.
#' 
#' @param affy.batch An object created by \code{readCelfiles}.
#' @param test Should the \code{.CEL} files be tested. When set to \code{TRUE}  
#'   bad \code{.CEL} files are automatically discarded.
#' @param quantile Specify a vector of quantiles that the data should be 
#'   normalized to.
#' @return Returns a \code{list} of length 9 with the slots:
#'   \item{exprs}{The cohort RMA pre-processed \eqn{log_2}-expression 
#'     \code{matrix}}
#'   \item{exprs.sc}{As slot \code{$exprs} but scaled to have zero median and 
#'     unit variance.}
#'   \item{exprs.sc.mean}{As slot \code{$exprs} but scaled to have zero mean and 
#'     unit variance.}
#'   \item{quantile}{The values defining the distribution used in quantile 
#'     normalization.}
#'   \item{alpha}{The alpha values of the RMA summarization.}
#'   \item{sd}{A \code{numeric} of the standard errors of each feature.}
#'   \item{median}{A \code{numeric} with the medians for each feature.}
#'   \item{mean}{A \code{numeric} with the means for each feature.}
#'   \item{bad}{A \code{character} giving the 'bad' arrays. \code{NULL} if no
#'     bad arrays are found or when \code{test = FALSE}}
#' @seealso \code{\link{rmaReference}} for one-by-one reference based RMA 
#'   normalization.
#' @references 
#'   \url{http://hemaClass.org}
#' @author 
#'   Steffen Falgreen <sfl (at) rn.dk> \cr 
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' # List .CEL files bundled with hemaClass
#' files <- list.files(system.file("extdata/celfiles", package = "hemaClass"), 
#'                     full.names = TRUE)
#' affy.batch <- readCelfiles(filenames = files[1:3]) # Read three first files
#' 
#' # RMA normalize
#' affy.rma <- rmaPreprocessing(affy.batch) 
#' str(affy.rma)
#' @import preprocessCore
#' @importFrom matrixStats rowMedians
#' @export
rmaPreprocessing <- function(affy.batch, test = FALSE, quantile = NULL) {
  # Get the probeset information
  probesets <- affy.batch$probesets
  
  # Get the pm probes
  ref.pm <- affy.batch$exprs
  
  # Background correction  
  ref.pm <- preprocessCore::rma.background.correct(ref.pm, copy = TRUE)
  colnames(ref.pm) <- colnames(affy.batch$exprs)
  rownames(ref.pm) <- rownames(affy.batch$exprs)
  
  contin <- TRUE
  if (test == TRUE) {
    wh  <- colSums(!is.finite(ref.pm)) > 0
    wh2 <- colSums(is.na(ref.pm)) > 0 
    
    bad  <- colnames(affy.batch$exprs)[wh | wh2]
    good <- setdiff(colnames(affy.batch$exprs), bad)
    if (length(good) > 0) {
      ref.pm <- ref.pm[, good]
    } else {
      warning("None of the supplied microarrays passed the quality control")
      contin <- FALSE
    }
    if (length(bad) > 0) {
      warning("The following arrays were discarded: ", 
              paste(bad, collapse = ", "))
    }
  } else {
    bad <- NULL
  }
  
  if (contin) {
    # Quantile normalisation
    if (is.null(quantile)) {
      generateQuan <- 1
      quantile <- numeric(nrow(ref.pm))
    } else {
      generateQuan <- 0
    }
      
    affy.batch <- RMA_norm(ref.pm, quantile, generateQuan)
    
    # log2 transformation of the data
    ref.pm.log <- log2(affy.batch$exprs)
    
    # Sort the data according the probesets
    #ref.pm.log <- ref.pm.log[paste(unlist(probesets)),]
    
    # Use the Rcpp based median polish to estimated expression levels 
    tmp <- RMA_sum(ref.pm.log, probesets, rownames(ref.pm.log), 
                   colnames(ref.pm.log))
    
    # Estimate median for later centralisation
    ref.median <- matrixStats::rowMedians(as.matrix(tmp$exprs))
    names(ref.median) <- rownames(tmp$exprs)
    
    # Estimate standard deviations for later normalisation
    ref.sd <- rowSds(as.matrix(tmp$exprs))
    
    ref.mean <- rowMeans(as.matrix(tmp$exprs))
    exprs.sc <- (tmp$exprs - ref.median) / ref.sd
    exprs.sc.mean <- (tmp$exprs - ref.mean) / ref.sd
   
    return(list(exprs = tmp$exprs, 
                exprs.sc = exprs.sc, 
                exprs.sc.mean = exprs.sc.mean, 
                quantile = affy.batch$quantile, 
                alpha = tmp$alpha, 
                sd = ref.sd, 
                median = ref.median, 
                mean = ref.mean,
                bad = bad))
  } else {
    
    return(bad)
    
  }
}

