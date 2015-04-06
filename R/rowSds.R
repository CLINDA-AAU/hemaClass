#' Calculation of standard deviations and variance
#' 
#' Calculate standard deviations and variance for each row or column of a 
#' matrix.
#' @rdname rowSds
#' @aliases colSds rowVars colVars
#' @param x A \code{matrix}.
#' @param ... Addtional parameters
#' @return A \code{vector} of variances or standard deviations of each 
#'   row or column of matrix \code{x}.
#' @author  
#'   Steffen Falgreen <sfl (at) rn.dk> \cr 
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' x <- matrix(rnorm(30), 5, 6)
#' rowSds(x)
#' colSds(x)
#' rowVars(x)
#' colVars(x)                                                                                                             
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
