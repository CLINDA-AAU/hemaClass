#' Read \code{.CEL} files into a matrix
#' 
#' Affymetrix \code{.CEL} files given in the argument filenames are read 
#' into a \code{matrix} in R.
#' 
#' @param filenames File names in a character vector.
#' @param cdfname Used to specify the name of an alternative cdf package. If set
#'   to \code{NULL}, then the usual cdf package based on Affymetrix' mappings 
#'   will be used.
#' @param rm.mask Should the spots marked as \code{'MASKS'} set to \code{NA}?
#' @param rm.outliers Should the spots marked as \code{'OUTLIERS'} set to 
#'   \code{NA}?
#' @param rm.extra If \code{TRUE}, then overrides what is in \code{rm.mask} and 
#'   \code{rm.oultiers}.
#' @param verbose logical. Should file names be written as they are read into R.
#' @param which A \code{character} determining if \code{pm}, \code{mm}, or 
#'   \code{both} probes be stored in matrices.
#'   
#' @return Expression matrix consisting of all \code{pm} probes.
#' @details Load \code{.CEL} files into a matrix.
#' @references Reference to the hemaClass.com paper
#' @author 
#'   Steffen Falgreen <sfl (at) rn.dk> \cr 
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' 
#' # Read hgu133plus2 .CEL files into R 
#' \donttest{
#' files <- dir(system.file("extdata/celfiles", package = "hemaClass"), 
#'              full.names = TRUE)
#' CEL.data <- readCelfiles(files) 
#' }
#' 
#' @seealso For preprocessing of the cel files see 
#'   \code{\link{rmaPreprocessing}}.
#' @import affy
#' @export
readCelfiles <- function(filenames, cdfname = NULL, rm.mask = FALSE, 
                         rm.outliers = FALSE, rm.extra = FALSE, verbose = FALSE, 
                         which = c("pm", "mm", "both")) {
  
  which <- match.arg(which)
  
  filenames <- as.character(filenames)
  
  if (verbose)
    cat("Reading", filenames[1], "to get header information.\n")
  
#  headdetails <- .Call("ReadHeader", filenames[1], PACKAGE="affyio")
  headdetails <- affyio::read.celfile.header(as.character(filenames[[1]]))
  dim.intensity <- headdetails[[2]]
  ref.cdfName <- headdetails[[1]]
  
  if(is.null(cdfname))
    cdfname <- ref.cdfName
  
  Data <- new("AffyBatch", cdfName = cdfname, 
              annotation = cleancdfname(ref.cdfName, addcdf = FALSE))
  
  probesets <- affy::indexProbes(Data)
  
  cdfInfo <- as.list(affy::getCdfInfo(Data))
  cdfInfo <- cdfInfo[order(names(cdfInfo))]
  
#   exprs<- .Call("read_probeintensities", filenames, rm.mask, rm.outliers, 
#                 rm.extra, ref.cdfName, dim.intensity, verbose, cdfInfo, 
#                 which, PACKAGE = "affyio")

  exprs <- affyio::read.celfile.probeintensity.matrices(filenames = filenames,
    cdfInfo = cdfInfo, rm.mask =  rm.mask, rm.outliers = rm.outliers, 
    rm.extra = rm.extra, verbose = verbose, which = which)
  
  rownames(exprs$pm) <- unlist(probesets)
  colnames(exprs$pm) <- basename(colnames(exprs$pm))
  
  return(list(exprs = exprs$pm, probesets = probesets, cdfname = cdfname, 
       annotation = cleancdfname(ref.cdfName, addcdf = FALSE)))
}
