#' Read cell files into a matrix
#' 
#' Cel files given in the argument filenames are read into a matrix in R.
#' 
#' @param filenames File names in a character vector.
#' @param cdfname Used to specify the name of an alternative cdf package. If set
#'   to NULL, then the usual cdf package based on Affymetrix' mappings will be
#'   used.
#' @param rm.mask Should the spots marked as 'MASKS' set to NA?
#' @param rm.outliers Should the spots marked as 'OUTLIERS' set to NA?
#' @param rm.extra If TRUE, then overrides what is in rm.mask and rm.oultiers.
#' @param verbose Should file names be written as they are read into R.
#' @param which should the pm, mm, or both be stored in matrices.
#'   
#' @return Expression matrix consisting of all pm probes.
#' @details Load cel files into a matrix.
#' @references Reference to the hemaClass.com paper
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' 2+2
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
