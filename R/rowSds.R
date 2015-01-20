#' Calculation of Standard deveations.
#' 
#' Calculation of Standard deveations.
#' @rdname rowSds
#' @aliases 
#'   colSds
#'   rowVars
#'   colVars
#' @param x A matrix.
#' @param ... Addtional parameters
#' @return The standard deviation of each row or column of matrix x.
#' @details The function calculates the standard deviation row wise for matrix x. 
#' @references Reference to the hemaClass paper.
#' @author Steffen Falgreen <sfl (at) rn.dk> \cr Anders Ellern Bilgrau <abilgrau
#' (at) math.aau.dk>
#' @examples
#' 2+2
#'                                                                                                                       
#' @export
rowSds <- function (x, ...) {
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(sqrt(rowSums((x - rowMeans(x, ...))^2, ...)/(n - 1)))
}

#' @rdname rowSds
#' @export
colSds <- function (x, ...) {
  n = colSums(!is.na(x))
  n[n <= 1] = NA
  return(sqrt(colSums((x - colMeans(x, ...))^2, ...)/(n - 1)))
}

#' @rdname rowSds
#' @export
rowVars <- function (x, ...) {
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return((rowSums((x - rowMeans(x, ...))^2, ...)/(n - 1)))
}

#' @rdname rowSds
#' @export
colVars <- function (x, ...) {
  n = colSums(!is.na(x))
  n[n <= 1] = NA
  return((colSums((x - colMeans(x, ...))^2, ...)/(n - 1)))
}
