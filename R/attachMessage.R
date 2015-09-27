#' Function for generating welcome message
#' 
#' Write the welcome message you want to give when the package is loaded
#' 
#' @param libname An object created by \code{readCelfiles}.
#' @param pkgname Should the \code{.CEL} files be tested. When set to \code{TRUE}  
#'   bad \code{.CEL} files are automatically discarded.
#' @return Message when loading package.
#' @details Load \code{.CEL} files into a matrix.
#' @references 
#'   \code{hemaClass.com}
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr 
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        paste("\n\nWelcome to hemaClass\n",
              "    This package performs one-by-one or reference based\n",
              "    pre-processing of gene expression data\n", sep = ""))
}
