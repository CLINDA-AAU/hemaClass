#' RMA pre-process \code{.CEL} files read
#'
#' Pre-process \code{.CEL} files according to the one-by-one RMA method
#' using a reference build using \code{\link{rmaPreprocessing}}.
#'
#' @param affy.batch An \code{affy.batch} object, usually created by
#'   \code{\link{readCelfiles}}.
#' @param reference An RMA reference object created by
#'   \code{\link{rmaPreprocessing}}.
#' @param test Should the supplied \code{.CEL} files be qualtiy tested. When
#'  set to \code{TRUE} bad \code{.CEL} files are automatically discarded.
#' @return Returns a \code{list} of length 3 with the slots:
#'   \item{exprs}{The one-by-one RMA pre-processed \eqn{log_2}-expression
#'     \code{matrix}}
#'   \item{exprs.sc}{As slot \code{$exprs} but scaled to have zero median and
#'     unit variance.}
#'   \item{exprs.sc.mean}{As slot \code{$exprs} but scaled to have zero mean and
#'     unit variance.}
#' @seealso
#'   \code{\link{rmaPreprocessing}}, \code{\link{readCelfiles}}
#' @references
#'   \url{http://hemaClass.org}
#' @author
#'   Steffen Falgreen <sfl (at) rn.dk> \cr
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' # List .CEL files bundled with hemaClass
#' files <- list.files(system.file("extdata/celfiles", package = "hemaClass"),
#'                     full.names = TRUE)
#' affy.batch <- readCelfiles(files) # Read in the .CEL files
#'
#' # Build references
#' ref.affy   <- rmaPreprocessing(affy.batch)
#' ref.affy.2 <- rmaPreprocessing(affy.batch, quantile = ref.affy$quantile)
#' all(ref.affy.2$exprs - ref.affy$exprs < 0.00000001)
#'
#' # Or use build-in:
#' ref.affy <- readCHEPRETROreference()
#' 
#' # RMA one-by-one pre-process using the reference:
#' user.affy  <- rmaReference(affy.batch, ref.affy)
#' @import preprocessCore
#' @export
rmaReference <- function(affy.batch, reference, test = FALSE) {
  # Get the probeset information
  probesets <- affy.batch$probesets

  # Get the pm probes
  ref.pm <- affy.batch$exprs
  
  # Background correction each column of a matrix
  ref.pm <- preprocessCore::rma.background.correct(ref.pm, copy = TRUE)
  dimnames(ref.pm) <- dimnames(affy.batch$exprs)  # Keep dimension names
  
  # Test for bad array quality
  if (test) {
    wh   <- colSums(!is.finite(ref.pm)) > 0
    bad  <- colnames(affy.batch$exprs)[wh]
    good <- setdiff(colnames(ref.pm), bad)
    if (length(good) > 0) {
      ref.pm <- ref.pm[, good]
    } else {
      warning("None of the supplied microarrays passed the quality control.")
      return(bad)
    } 
    if (length(bad) > 0) {
      warning("The following arrays were discarded: ",
              paste(bad, collapse = ", "))
    }
  }

  # Normalisation and summarisation according to reference
  ans <- userRMA(ref.pm, 
                 probesets = probesets, 
                 colnames = colnames(ref.pm), 
                 quantile = reference$quantile,  
                 alpha = reference$alpha[rownames(ref.pm)])
  
  # Scale and center the RMA normalised data
  nms <- rownames(ans$exprs) # Nessesary for locale specific ordering
  ans$exprs.sc      <- (ans$exprs - reference$median[nms])/reference$sd[nms]
  ans$exprs.sc.mean <- (ans$exprs - reference$mean[nms])/reference$sd[nms]
  
  # Calculate RLE
  ans$RLE       <- (ans$exprs - reference$median[nms])
  ans$RLE.stats <- rleSTATS(ans$RLE)
  
  return(ans)
}

rleSTATS=function(rle){
  rle_mean=colMeans(rle)
  rle_median=matrixStats::colMedians(rle)
  rle_iqr=apply(rle,2,matrixStats::iqr)
  rle_lower=apply(rle,2,function(x){quantile(x, probs=0.025)})
  rle_upper=apply(rle,2,function(x){quantile(x, probs=0.975)})
  
  results=cbind(rle_mean,rle_median,rle_iqr,rle_lower,rle_upper)
  colnames(results)=c("RLE mean","RLE Median","RLE IQR","RLE 2.5%","RLE 97.5%")
  return(results)
}