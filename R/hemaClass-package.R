#' hemaClass: Classification tool for haematological cancers
#'
#' An R-package for DNA microarray classification.
#' @author 
#'   Steffen Falgreen Larsen <sfl (at) rn.dk> \cr
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @docType package
#' @name hemaClass-package
#' @aliases hemaClass-package HEMACLASS hemaclass
#' @useDynLib hemaClass
#' @importFrom Rcpp evalCpp
#' @importFrom RcppArmadillo armadillo_set_seed
#' @examples
#' # See the following for all exported functions
#' ls("package:hemaClass")
#' 
#' # To run the hemaClass web-interface:
#' require(gdata)
#' require(survival)
#' require(shinysky)
#' runHemaClass()
NULL

# About "importFrom Rcpp evalCpp": and RcppArmadillo
#
# http://adv-r.had.co.nz/Rcpp.html#adding-rcpp-to-an-existing-package-rcpp-0.10.7
# “We need to import something (anything) from Rcpp so that 
# internal Rcpp code is properly loaded. In an ideal world, 
# we would just need to use the LinkingTo but this is not
# enough.”