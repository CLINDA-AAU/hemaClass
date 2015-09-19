hemaClass: Classification tool for haematological diseases. 
=================================

The R-package `hemaClass` is a set of tools used for classification of hematological cancers using DNA microarrays.

The packages is features (or, is planned to feature):
* Reference based RMA normalisation ✓
* Prediction of drug resistance

The currently featured platforms are:
* Affymetrix GeneChip HG-U133 Plus 2.0 (`"u133plus2"`)
* But many more are to come... (hopefully)

Installation
------------
If you wish to install the latest version of `hemaClass` directly from the master branch here at GitHub, run 

```R
# Install necessary packages 
# First from bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite(c("affy", "affyio", "preprocessCore"))

# Then from CRAN
install.packages(c("shiny", "matrixStats", "Rcpp", "RcppArmadillo", 
                   "RcppEigen", "testthat", "WriteXLS", "RLumShiny", "gdata"))

# From GitHub 
install.packages("devtools")
devtools::install_github("AnalytixWare/ShinySky")

# Finally the package is installed.
devtools::install_github("oncoclass/hemaClass", dependencies = TRUE)
```

`hemaClass` is still under development and should be considered unstable. Be sure that you have the [package development prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) if you wish to install the package from the source.

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
A small benchmark goes here!

References
----------
References goes here!
