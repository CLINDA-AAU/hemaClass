hemaClass: Classification tool for haematological diseases
==========================================================

The package **hemaClass** for the programming language R is a set of tools used for classification of hematological cancers using DNA microarrays. The package features one-by-one and reference based RMA normalisation, a proposed alternatives to regular cohort based RMA normalization. Additionally, it features oncogenomic classification and prediction of drug resistance of Diffuse Large B-Cell Lymphomas.

An easy-to-use **shiny** web application is incorporated into the package and available online at [hemaclass.org](http://hemaclass.org) or as a local instance via `runHemaClass()` in R.

The currently only featured platform is Affymetrix GeneChip HG-U133 Plus 2.0 (`"u133plus2"`) but more are planned.

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
---------------
Please confer the vignette to **hemaClass** which can be found via `vignette(package = "hemaClass")` or on the help page at [hemaclass.org](http://hemaclass.org).
It can also be directly read at [github.](https://github.com/oncoclass/hemaClass/blob/master/vignettes/howto.Rmd)


References
----------

1. Steffen Falgreen, Anders Ellern Bilgrau, Jonas Have; **"hemaClass: Online classification of gene expression profiles in hematological cancers."** (2014) http://github.com/falgreen/hemaClass

2. Steffen Falgreen, Anders Ellern Bilgrau, Lasse Hjort Jakobsen, Jonas Have, Kasper Lindblad Nielsen, Tarec Christoffer El-Galaly, Julie Støve Bøker, Alexander Schmitz, Hans Erik Johnsen, Karen Dybkær, and Martin Bøgsted; hemaClass.org: An online based diffuse large B-cell lymphoma classification tool. (2015) In preperation for BMC genomics.

3. Dybkær K, Bøgsted M, Falgreen S, Bødker JS et al. *"Diffuse Large B-cell Lymphoma Classification System That Associates  Normal B-cell Subset Phenotypes with Prognosis."* Journal of Clinical Oncology 33, no. 12 (2015): 1379-1388. (GSE56315)
       
4. Falgreen S, Dybkær K, Young KH, Xu-Monette ZY et al. *"Predicting response to multidrug regimens in cancer patients using cell line experiments and regularised regression models."* BMC cancer 15, no. 1 (2015): 235.

5. Laursen, MB, Falgreen S, Bødker JS, Schmitz A, et al. *"Human B-cell cancer cell lines as a preclinical model for studies of drug effect in diffuse large B-cell lymphoma and multiple myeloma."* Experimental Hematology 42, no. 11 (2014): 927-938.

