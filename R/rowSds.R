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
  return(sqrt(rowVars(x)))
}

#' @rdname rowSds
#' @export
colSds <- function (x, ...) {
  return(rowSds(t(x)))
}

#' @rdname rowSds
#' @export
rowVars <- function (x, ...) {
  if (ncol(x) == 0L) {
    return(rep(NA_real_, nrow(x)))
  }
  n <- rowSums(!is.na(x))
  n[n <= 1] <- NA_real_
  return(rowSums((x - rowMeans(x, ...))^2, ...)/(n - 1))
}

#' @rdname rowSds
#' @export
colVars <- function (x, ...) {
  return(rowVars(t(x)))
}
