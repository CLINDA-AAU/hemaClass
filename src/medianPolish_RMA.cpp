#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List RMA_sum(arma::mat x2, Rcpp::List probesets, CharacterVector order, CharacterVector colnames) {
  
  int maxite = 10;
  int m2 = x2.n_cols;
  double eps = 1e-2;
  
  int n = 0;
  int m = 0;
  
  double tm = 0; // total median
  double eps2 = 0;
  double delta = 0;
  double oldsum = 0;
  double newsum = 0;
  
  NumericVector alpha(order.size());
  alpha.names() = order;
  CharacterVector probesets2 = probesets.names();
  
  double count = 0;
  
  NumericMatrix xsum (probesets2.size(), m2);
  
  Rcpp::List dimns = Rcpp::List::create(probesets2, colnames);
  
  xsum.attr("dimnames") = dimns;
  
  
  arma::rowvec cdelta(m2, arma::fill::none);
  
  for(int l = 0; l < probesets2.size(); ++l){
    CharacterVector probes = probesets(l);
    IntegerVector x5(probes.size()); 
     
    x5 = seq(count, count + probes.size());
    count += probes.size();
    arma::mat x(probes.size(), m2);
    arma::mat x3(probes.size(), m2);
        
    for(int j = 0; j < probes.size(); ++j){
        x(j, arma::span::all) = x2(x5(j), arma::span::all);
    }
    x3 = x;
    n = x.n_rows;
    m = x.n_cols;
    
    tm = 0; // total median
    eps2 = 0;
    delta = 0;
    oldsum = 0;
    newsum = 0;
    
    arma::colvec rm(n, arma::fill::zeros);
    arma::rowvec cm(m, arma::fill::zeros);
    arma::colvec rdelta(n, arma::fill::none); 
    
       
    for (int k=0; k<maxite; k++) {
      rdelta = arma::median(x, 1);
      for (int j=0; j<m; j++) {
        x(arma::span::all, j) -= rdelta; // Sweep the colums
      }
      rm += rdelta;
      
      delta = median(cm);
      cm -= delta;
      tm += delta;
      
      cdelta = arma::median(x, 0);
      for (int i=0; i<n; i++) {
        x(i, arma::span::all) -= cdelta; // Sweep the rows
      }
      cm += cdelta;
      
      delta = median(rm);
      rm -= delta;
      tm += delta;

      // Check for convergence
      newsum = accu(abs(x));
      eps2 = eps*newsum;
      if (newsum==0)
      break;
      if (std::abs(newsum-oldsum)<eps2)
      break;
      
      oldsum = newsum;
      
    } 
    
    for(int j = 0; j < probes.size(); ++j){
      alpha(x5(j)) =  rm(j);
    }
    
    for (int j=0; j<m; j++) {
        x3(arma::span::all, j) -= rm; // Sweep the rows
    }
    
    cdelta = arma::median(x3, 0);
    
    for (int j=0; j<m; j++) {
      xsum(l, j) = cdelta(j);
    }  
    
  }
  
  return List::create(
    Named("alpha") = alpha,
    Named("exprs") = xsum,
    Named("cdelta") = cdelta
  );       
}

