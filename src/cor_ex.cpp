// Only include aux_functions.h which pulls in RcppArmadillo.h,
// RcppEigen.h, and Rcpp.h

#include "auxiliary_functions.h"

//// [[Rcpp::depends(RcppArmadillo)]] // Uncomment when sourceCpp()ing
//// [[Rcpp::depends(RcppEigen)]]

//' Marginal correlation matrix
//'
//' Various workhorse functions to compute the marginal (or unconditional)
//' correlations (and cross-correlation) estimates efficiently.
//' They are (almost)
//' equivalent implementations of \code{\link[stats]{cor}} in Rcpp,
//' RcppArmadillo, and RcppEigen.
//'
//' @param X A numeric matrix.
//' @return
//'   The \code{corXX} familiy returns a numeric correlation matrix of size
//'   \code{ncol(X)} times \code{ncol(X)}.
//'
//'   The \code{xcorXX} family returns a numeric cross-correlation matrix
//'   of size \code{ncol(X)} times \code{ncol(Y)}.
//' @details
//'   Functions almost like \code{\link{cor}}.
//'   For the \code{xcorXX} functions, the \code{i}'th and \code{j}'th
//'   entry of the output matrix is the correlation between \code{X[i, ]} and
//'   \code{X[j, ]}.
//' @author Steffen Falgreen Larsen, Anders Ellern Bilgrau
//' @export
// [[Rcpp::export]]
arma::mat cor(const arma::mat & X) {
  //arma::mat cor = arma::cor(X, 0);
  // Ensure 1 in the diagonal if NAs in X
  //cor.diag() = arma::ones<arma::vec>(cor.n_rows); 
  return arma::cor(X, 0);
}