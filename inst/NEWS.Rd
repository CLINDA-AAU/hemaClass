\name{NEWS}
\title{News for Package \pkg{hemaClass}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\issue}{\href{https://github.com/oncoclass/hemaClass/issues/#1}{issue ##1}}

\section{Changes in version 1.0.3 - Oct. 2015}{
  \itemize{
    \item Patch to v1.0.4:
    \itemize{
      \item Added the reset button discussed in \issue{39}.
      \item Removed line numbers as mentioned in \issue{40}.
    }
    \item Patch to v1.0.3:
    \itemize{
      \item Fixed error in \issue{37}.
    }
    \item Patch to v1.0.2:
    \itemize{
      \item Dexamethasone incorrectly abbreviated as P which is usually used for
            prednisone. 
    }
    \item Patch to v1.0.1:
    \itemize{
      \item Fixed spurious warnings on website when using build-in data 
            (\issue{32}).
      \item Stress that IE9 and older are not supported. See \issue{33} and 
            \issue{34}.
    }
  }
}


\section{Changes in version 1.0 - Oct. 2015}{
  First version of \pkg{hemaClass} and \href{http://hemaclass.org}{hemaClass.org}!
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
      \item Dexamethasone (Experimental!)
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


