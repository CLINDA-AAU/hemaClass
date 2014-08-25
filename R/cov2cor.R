#' A conversion from covariance to correlation matrix
#' 
#' @param S A covariance matrix
#' @return A correlation matrix of the same size as \code{S}
#' @author 
#'   Steffen Falgreen Larsen
#'   Anders Ellern Bilgrau
#' @details Some more details
#' @note Some note and warnings of use.
#' @examples
#' X <- replicate(5, rnorm(10))
#' S <- cov(X)
#' cov2cor(S)
#' @export
cov2cor <- function(S) {
  return(stats::cov2cor(S))
}