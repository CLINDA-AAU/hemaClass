context("Test quantile normalisation, test-RMA_norm")

# Define the old RMA normalizer
code <- '
List RMA_norm_old(const NumericMatrix x2, arma::vec quantile2, 
                  int generateQuan) {
  NumericMatrix x = clone(x2);
  int n = x.nrow();
  int m = x.ncol();
  arma::mat xnorm(n,m);
  NumericMatrix xsort(n,m);

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
  if (generateQuan==1) quantile = arma::mean(xnorm, 1);
  if (generateQuan==0) quantile = quantile2;
  
  NumericVector h2(n);  
  
  for (int j = 0; j < m; ++j) {
    h2 = x(_, j);
    h2 = h2.sort();
      
    NumericVector revh1 = clone<NumericVector>(h2);
    std::reverse(revh1.begin(), revh1.end());
      
    NumericVector revx = clone<NumericVector>(x(_, j));
    std::reverse(revx.begin(), revx.end());
      
    match2 = match(revx, revh1);    
    IntegerVector revmatch2 = clone<IntegerVector>(match2);
    std::reverse(revmatch2.begin(), revmatch2.end());
      
    match2 = match(x(_, j), h2);
    for (int i = 0; i < n; ++i) {
      xsort(i, j) = (quantile(revh3(revmatch2(i)-1)-1) +
       quantile(h3(match2(i)-1)-1))/2 ;
    }
  }
  
  return Rcpp::List::create(
    Rcpp::Named("exprs") = xsort,
    Rcpp::Named("quantile") = quantile
  );       
}'

library("Rcpp")
cppFunction(code, depends = "RcppArmadillo")


# # R version
# quantilenormalize <- function(exprs) {
#   x <- exprs
#   N <- ncol(x)
#   p <- nrow(x)
#   xorder <- apply(x, 2, order)
#   xsort <- apply(x, 2, sort)
#   xsort_tmp <- xsort/sqrt(N)
#   xnorm <- matrix(0, p, N)
#   for (i in 1:N) {
#     xnorm[,i] <- xsort_tmp[order(xorder[,i]), i]
#   }
#   return(xnorm)
# }

# List .CEL files bundled with hemaClass
files <- list.files(system.file("extdata/celfiles", package = "hemaClass"), 
                    full.names = TRUE)
affy.batch <- readCelfiles(filenames = files) # Read three first files

# RMA normalize
probesets <- affy.batch$probesets
ref.pm <- affy.batch$exprs # Get the pm probes
ref.pm <- preprocessCore::rma.background.correct(ref.pm, copy = TRUE)
dimnames(ref.pm) <- dimnames(affy.batch$exprs)

# Quantile normalisation
# generateQuan <- FALSE
generateQuan <- TRUE
quantile <- numeric(nrow(ref.pm))

rma.normalized     <- hemaClass:::RMA_norm(ref.pm, quantile, generateQuan)
rma.normalized_old <- RMA_norm_old(ref.pm, quantile, generateQuan)
# rma.normalized.R   <- quantilenormalize(ref.pm)

test_that("Current RMA_norm agrees with old version", {
  expect_equal(rma.normalized, rma.normalized_old)  
})


