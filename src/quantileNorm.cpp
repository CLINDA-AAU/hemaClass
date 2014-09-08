#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List RMA_norm(const NumericMatrix x2, arma::vec quantile2, int generateQuan) {
  
  NumericMatrix x = clone(x2);
  
  int n = x.nrow();
  int m = x.ncol();
  
  arma::mat xnorm(n,m);
  NumericMatrix xsort(n,m);
  
  
  //  Rcpp::List dimns = Rcpp::List::create(probesets2, colnames);
  
  xsort.attr("dimnames") = x.attr("dimnames");
  
  IntegerVector match2 = seq_len(n);
  IntegerVector h3 = seq_len(n);
  IntegerVector revh3 = clone<IntegerVector>(h3);
  std::reverse(revh3.begin(), revh3.end());
  
  arma::vec h1(n);  
  
  for(int j = 0; j < m; ++j){
    h1 = x(_, j);
    xnorm(arma::span::all, j) = sort(h1);
  }
  
  arma::vec quantile(n, arma::fill::none);
  
  if (generateQuan==1)
    quantile = arma::mean(xnorm, 1);
  if (generateQuan==0)
    quantile = quantile2;
  
  NumericVector h2(n);  
  
  for(int j = 0; j < m; ++j){
    h2 = x(_, j);
    h2 = h2.sort();
    //    
    //    h2 =xnorm(arma::span::all, j);
    
    NumericVector revh1 = clone<NumericVector>(h2);
    std::reverse(revh1.begin(), revh1.end());
    
    NumericVector revx = clone<NumericVector>(x(_, j));
    std::reverse(revx.begin(), revx.end());
    
    match2 = match(revx, revh1);    
    IntegerVector revmatch2 = clone<IntegerVector>(match2);
    std::reverse(revmatch2.begin(), revmatch2.end());
    
//  max  
        for(int i = 0; i < n; ++i){
          xsort(i, j) = quantile(revh3(revmatch2(i)-1)-1) ;
        }

//    min
//    match2 = match(x(_, j), h2);
//    for(int i = 0; i < n; ++i){
//      xsort(i, j) = quantile(h3(match2(i)-1)-1);
//    }

//   mean of min og max
// match2 = match(x(_, j), h2);
//   for(int i = 0; i < n; ++i){
//          xsort(i, j) = (quantile(revh3(revmatch2(i)-1)-1) + quantile(h3(match2(i)-1)-1))/2 ;
//        }
        
        
  }  
  
  return Rcpp::List::create(
    Rcpp::Named("exprs") = xsort,
    Rcpp::Named("quantile") = quantile
    );       
}