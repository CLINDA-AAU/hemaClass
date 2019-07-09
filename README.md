hemaClass: Classification tool for haematological diseases
==========================================================

The package **hemaClass** for the programming language R is a set of tools used for classification of hematological cancers using DNA microarrays. The package features one-by-one and reference based RMA normalisation, a proposed alternatives to regular cohort based RMA normalization, and oncogenomic classification and prediction of drug resistance of Diffuse Large B-Cell Lymphomas (DLBCL) and Multiple Myeloma (MM).

An easy-to-use **shiny** web application is incorporated into the package and available online at [hemaclass.org](http://hemaclass.org) or as a local instance via `runHemaClass()` in R. The **hemaClass** package can naturally also be used programatically as a regular R-package.
Please do not hessitate to report bugs, suggestions, comments, and other issues for the **hemaClass** website or package via [`bug.report(package = "hemaClass")`](https://github.com/oncoclass/hemaclass/issues/new).

The standard BAGS and REGS implemented in the shiny GUI only supports Affymetrix GeneChip HG-U133 Plus 2.0 (`"u133plus2"`). Prediction of BAGS classes uses nanostring data is supported in the R terminal with the BAGS2clinic() function.

Installation
------------
If you wish to install the latest version of `hemaClass` directly from the master branch here at GitHub, run 

```r
# Install necessary packages
# First from bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("affy", "affyio", "preprocessCore"))

# Then from CRAN
install.packages(c("shiny", "matrixStats", "Rcpp", "RcppArmadillo", "stringi",
                   "testthat", "WriteXLS", "RLumShiny", "gdata", "devtools"))

# From GitHub and finally the package:
devtools::install_github("AnalytixWare/ShinySky")
devtools::install_github("HaemAalborg/hemaClass", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

`hemaClass` is still under development and should be considered unstable. Be sure that you have the [package development prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) if you wish to install the package from the source.

**Note:** The interface and function names may still see significant changes and
modifications!


Using hemaClass
---------------
Please confer the vignette to **hemaClass** which can be found via `vignette("howto")` or on the help page at [hemaclass.org](http://hemaclass.org).
It can also be directly read at [github.](https://github.com/oncoclass/hemaClass/blob/master/vignettes/howto.Rmd)


References
----------

1. Steffen Falgreen, Anders Ellern Bilgrau, Jonas Have; *"hemaClass: Online classification of gene expression profiles in hematological cancers."* (2014) http://github.com/falgreen/hemaClass

2. Steffen Falgreen, Anders Ellern Bilgrau, Rasmus Froberg Broendum, Lasse Hjort Jakobsen, Jonas Have, Kasper Lindblad Nielsen, Tarec Christoffer El-Galaly, Julie Stoeve Boedker, Alexander Schmitz, Hans Erik Johnsen, Karen Dybkaer, and Martin Boegsted; *"hemaClass.org: Online One-By-One Microarray Normalization and Classification of Hematological Cancers for Precision Medicine."*" PLoS ONE Vol. 11, Issue 10 (2016)

3. Dybkaer K, Boegsted M, Falgreen S, Boedker JS et al. *"Diffuse Large B-cell Lymphoma Classification System That Associates  Normal B-cell Subset Phenotypes with Prognosis."* Journal of Clinical Oncology 33, no. 12 (2015): 1379-1388. (GSE56315)
       
4. Falgreen S, Dybkaer K, Young KH, Xu-Monette ZY et al. *"Predicting response to multidrug regimens in cancer patients using cell line experiments and regularised regression models."* BMC cancer 15, no. 1 (2015): 235.

5. Laursen, MB, Falgreen S, Boedker JS, Schmitz A, et al. *"Human B-cell cancer cell lines as a preclinical model for studies of drug effect in diffuse large B-cell lymphoma and multiple myeloma."* Experimental Hematology 42, no. 11 (2014): 927-938.

6. Michaelsen, T. Y. et al. (2018) *"A B-cell-associated gene signature classification of diffuse large B-cell lymphoma by NanoString technology."* Blood advances, 2(13): 1542-1546.