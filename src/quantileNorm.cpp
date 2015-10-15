#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List RMA_norm(const NumericMatrix x_usr, 
              arma::vec quantile_usr,
              const int generateQuan) {
  
  NumericMatrix x = clone(x_usr);
  int n = x.nrow();
  int m = x.ncol();
  
  // If generateQuan, sort each column and take mean of each row, 
  // else use user supplied quantile
  arma::vec quantile(n, arma::fill::none);
  if (generateQuan == 1) {
    
    // Sort all columns of x
    arma::vec h1(n);  
    arma::mat xsort(n, m);
    for (int j = 0; j < m; ++j) {
      h1 = x(_, j);
      xsort(arma::span::all, j) = sort(h1);
    }
    
    quantile = arma::mean(xsort, 1);
  }
  
  if (generateQuan == 0) {
    
    quantile = quantile_usr;
    
  } 
  
  NumericMatrix xnorm(n, m);
  xnorm.attr("dimnames") = x.attr("dimnames");
  
  IntegerVector match2 = seq_len(n);
  IntegerVector h3 = seq_len(n);
  IntegerVector revh3 = clone<IntegerVector>(h3);
  std::reverse(revh3.begin(), revh3.end());
  
  NumericVector h2(n);  
  for (int j = 0; j < m; ++j) {
    h2 = x(_, j);
    h2 = h2.sort();

    // revh2 <- rev(h2)
    NumericVector revh2 = clone<NumericVector>(h2);
    std::reverse(revh2.begin(), revh2.end());
    
    // revx <- rev(x[, j])
    NumericVector revx = clone<NumericVector>(x(_, j));
    std::reverse(revx.begin(), revx.end());
    
    //  Match reversed vectors
    match2 = match(revx, revh2);    
    IntegerVector revmatch2 = clone<IntegerVector>(match2);
    std::reverse(revmatch2.begin(), revmatch2.end());
    
    // Mean of min og max, handle ties
    match2 = match(x(_, j), h2); // equiv to rank(x[, j])
    for (int i = 0; i < n; ++i) {
      xnorm(i, j) = 
        (quantile(h3(match2(i) - 1) - 1 ) +  // why not quantile(match2(i) - 1)? 
          quantile(revh3(revmatch2(i) - 1) - 1))/2;
    }
  }
  
  return List::create(Named("exprs") = xnorm, Named("quantile") = quantile);       
}
