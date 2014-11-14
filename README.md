hemaClass: Classification tool for haematological diseases. 
=================================

The R-package `hemaClass` is a set of tools used for classification of hematological cancers using DNA microarrays.

The packages is features (or, is planned to feature):
* Reference based RMA normalisation ✓
* Prediction of drug resistance

The currently featured platforms are:
* Affymetrix GeneChip HG-U133 Plus 2.0 (`"u133plus2"`)
Many more are to come...


Installation
------------
If you wish to install the latest version of `hemaClass` directly from the master branch here at GitHub, run 

```R
# Install necessary packages 

# First form bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("affy")
biocLite("affyio")
biocLite("preprocessCore")

# Then form CRAN
install.packages("shiny")
install.packages("matrixStats")
install.packages("Rcpp")
install.packages("RcppArmadillo")
install.packages("RcppEigen")
install.packages("testthat")

# Finally the package is installed.
#install.packages("devtools")  # Uncomment if devtools is not installed
devtools::install_github("Falgreen/hemaClass", 
                         auth_token = "...", 
                         dependencies = TRUE)
```

`hemaClass` is still under heavy development and should be considered unstable. Be sure that you have the [package development prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) if you wish to install the package from the source.

**Note:** The interface and function names may still see significant changes and
modifications!


Using hemaClass
----------
A small tutorial goes here!

### Conventions and interface
The supplied expression matrices to be converted are arrange in the tall 
configuration. I.e. we have features in the rows and samples in columns.

Benchmark 
---------
A small bencmark goes here!

References
----------
References goes here!
