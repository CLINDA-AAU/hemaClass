\name{NEWS}
\title{News for Package \pkg{hemaClass}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 1.0 - Oct. 2015}{
  \itemize{
    \item Manuscript titled "hemaClass.org: Online one-by-one normalization and
          classification of haematological malignancies" by Falgreen, Bilgrau, 
          Jakobsen et al., has now been submitted for publication.
    \item \pkg{hemaClass} functionality greatly expanded.
    \item The package and website now supports proper reference-based 
          one-by-one RMA pre-processing as well as regular cohort based RMA 
          pre-processing. See \code{\link{rmaPreprocess}} and 
          \code{\link{rmaReference}}.
    \item Supports various cell-of-origin classification systems 
          (see \code{help(BAGS)}):
    \itemize{
      \item B-Cell Associated Gene Signatures (BAGS)
      \item Activated B-Cell-like and Germinal Center B-cell-like
            classification (ABC/GCB)
      \item BAGS-extended ABC/GCB into ABC / GCB / GCB-CC/ GCB-CB
    }
    \item Supports various REsistance Gene Signatures (REGS) classifications 
          for (see \code{help(REGS)}):
    \itemize{
      \item Rituximab (R)
   		\item Cyclophosphamide (C)
      \item Doxorubicin (H)
      \item Vincristine (O)
      \item Dexamethasone (P) (Experimental!)
      \item Melphalan (M) 
    }
    \item \pkg{hemaClass}.org website functionality polished with 
          context-dependent feedback. To use the website, go to 
          \url{http://hemaClass.org} or see \code{help(runHemaClass)}.
  }
}

\section{Changes in version 0.1 - Aug. 2014}{
  \itemize{
    \item Basic classification functionality.
    \item \pkg{hemaClass} was created!
  }
}


