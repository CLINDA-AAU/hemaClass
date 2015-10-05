# The hemaClass welcome message display upon package load.
# 
# Steffen Falgreen <sfl (at) rn.dk>
# Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n\nWelcome to hemaClass\n",
    "  This package performs one-by-one or reference based RMA pre-processing\n",
    "  of gene expression data and classification of hematological cancers.\n",
    "  Please don't hessitate to contact us or report bugs and issues at\n",
    "   *  https://github.com/oncoclass/hemaClass/issues"
  )
}
