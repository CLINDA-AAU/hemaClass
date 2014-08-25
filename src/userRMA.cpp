#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List userRMA(const NumericMatrix x2, Rcpp::List probesets, CharacterVector colnames, NumericVector quantile, NumericVector alpha) {
  
  NumericMatrix x = clone(x2);
  CharacterVector probesets2 = probesets.names();
  
  int n = x.nrow();
  int m = x.ncol();
  int p = probesets2.size();
  
  NumericMatrix xnorm(n,m);
  
  arma::rowvec cdelta(m, arma::fill::none);
   
  IntegerVector match2 = seq_len(n);
  IntegerVector h5 = seq_len(n);
  IntegerVector h3 = seq_len(n);
  IntegerVector revh3 = clone<IntegerVector>(h3);
  std::reverse(revh3.begin(), revh3.end());
  
  NumericMatrix xsum (p, m);
  Rcpp::List dimns = Rcpp::List::create(probesets2, colnames);
  xsum.attr("dimnames") = dimns;
  
  NumericMatrix xsort(n,m);
  NumericVector h1 (p);  
  
  for(int j = 0; j < m; ++j){
    h1 = x(_, j);
    h1 = h1.sort();
    
    NumericVector revh1 = clone<NumericVector>(h1);
    std::reverse(revh1.begin(), revh1.end());
    
    NumericVector revx = clone<NumericVector>(x(_, j));
    std::reverse(revx.begin(), revx.end());
    
    match2 = match(revx, revh1);
    
    IntegerVector revmatch2 = clone<IntegerVector>(match2);
    std::reverse(revmatch2.begin(), revmatch2.end());
    
    for(int i = 0; i < n; ++i){
      xsort(i, j) = quantile(revh3(revmatch2(i)-1)-1);
    }
  } 
    
  for(int j = 0; j < m; ++j){
    xnorm(_,j) =  log(xsort(_,j)) / log(2) - alpha;
  }
  
  
  double count = 0;
  double n2 = 0;
  for(int l = 0; l < p; ++l){
    CharacterVector probes = probesets(l);
    n2 = probes.size();
    
    IntegerVector x5(n2);
    x5 = seq(count, count + probes.size());
    count += n2;
    
    arma::mat x3(n2, m);
    
    for(int j = 0; j < n2; ++j){
      cdelta =xnorm(x5(j), _);
        x3(j, arma::span::all) = cdelta;
    }    
    
    cdelta = arma::median(x3, 0);
    
    for (int j=0; j<m; j++) {
      xsum(l, j) = cdelta(j);
    } 
    
  }
  return Rcpp::List::create(
    Rcpp::Named("exprs") = xsum
    );       
}