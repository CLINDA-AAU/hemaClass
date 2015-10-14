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
#' # Use build-in:
#' ref.affy <- readCHEPRETROreference()
#' 
#' # RMA one-by-one pre-process using the reference:
#' user.affy  <- rmaReference(affy.batch, ref.affy)
#'
#' all(user.affy$exprs - ref.affy$exprs < 0.00000001)
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
  ans$exprs.sc      <- (ans$exprs - reference$median)/reference$sd
  ans$exprs.sc.mean <- (ans$exprs - reference$mean)/reference$sd
  
  return(ans)
}

