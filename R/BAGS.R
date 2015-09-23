#' Classification of cell of origin
#' 
#' Classification using the B-cell associated gene signatures (BAGS) classifier 
#' of Dybkaer et al. [1]. The BAGS classifier attemps to assign the given 
#' supplied Diffuse Large B-Cell Lymphomas (DLBCL) 
#' samples to one of five classes according to their resembalace to normal 
#' Naive (N), Centrocytes (CC), Centroblasts (CB), Memory (M), and 
#' Plasmablasts (PB) cells. 
#' 
#' @rdname BAGS
#' @aliases 
#'   BAGS
#'   BAGSClassifier
#'   BAGSclassifier 
#' @param new.data An expression \code{matrix}.
#' @param cut.spec A \code{numeric} value used to threshold the probabilities 
#'   and determine the class.
#' @param percent.classified A \code{numeric} value indicating the percentage of 
#'   patients that should be classified. An alternative to \code{cut.spec}.
#' @return A \code{list} of probabilities regarding each patients association 
#'   with each class, the determined class, and the used cut-off thresholds.
#' @details The function BAGS classifies DLBCL patients according to the cell 
#'   of origin for the tumor [1].
#' @references
#' [1] Dybkaer, Karen, Martin Boegsted, Steffen Falgreen, Julie S. Boedker, 
#' Malene K. Kjeldsen, Alexander Schmitz, Anders E. Bilgrau et al. 
#' "Diffuse large B-cell lymphoma classification system that associates 
#' normal B-cell subset phenotypes with prognosis." 
#' Journal of Clinical Oncology 33, no. 12 (2015): 1379-1388. 
#'
#' [Add hemaclass.org reference]
#' @author 
#'   Steffen Falgreen <sfl (at) rn.dk> \cr 
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' \donttest{
#' files <- dir(system.file("extdata/celfiles", package = "hemaClass"), 
#'              full.names = TRUE)
#' affyBatch <- readCelfiles(filenames = files)
#' 
#' # The cel files are pre-processed
#' affyRMA <- rmaPreprocessing(affyBatch)
#' 
#' # The function rmaPreprocessing returns median centered and scaled
#' # expression values in the slot exprs.sc.
#' 
#' # The slot exprs.sc.mean contains mean centered and scaled expression values.
#' # This scaling can also be achieved using the function microarrayScale.
#' affyRMA.sc <- microarrayScale(affyRMA$exprs, center = "median")
#' 
#' # We may now use the ABCGCB classifier
#' BAGS(affyRMA.sc)
#' }                                                                                                                  
#' @export
BAGS <- function(new.data, cut.spec = NULL, percent.classified = 85){
  
  new.data[is.na(new.data)] <- 0
  
  train.mat <- BAGSProbFun(new.data)
  
  if (nrow(new.data) > 1) {
    class <- colnames(train.mat)[apply(train.mat, 1, which.max)]
  } else {
    class <- names(which.max(train.mat))
  }
  
  if (nrow(new.data) > 1) {
    prob <- apply(train.mat, 1, max)
    if (is.null(cut.spec)) {
      cut <- quantile(prob, 1 - percent.classified / 100)    
    }
  } else {
    cut <- 0
    prob = max(train.mat)
  }
  
  if (!is.null(cut.spec)) {
    cut <- cut.spec
  }

  class <- factor(ifelse(prob < cut, "Unclassified", class),
                  levels = c("Naive", "Centroblast", "Centrocyte", 
                             "Memory", "Plasmablast", "Unclassified"))
  
  return(list(class = class, prob.mat = train.mat, prob = prob, cut = cut))
}

# Function for computing the BAGS probabilities
BAGSProbFun <- function(newx) {
  BAGS.coef <- readBAGSCoef()
  
  diff <- setdiff(row.names(BAGS.coef)[-1], rownames(newx))
  
  if (length(diff)) {
    missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                       dimnames = list(diff, colnames(newx) ))
    
    newx <- rbind(newx, missing)
    
    warning("The following probesets are missing:\n", 
            paste(diff, collapse = ", "))
  }
  
  x <- rbind(1, newx[row.names(BAGS.coef)[-1], , drop = FALSE])
  
  prob.mat <- matrix(ncol = 5, nrow = ncol(x))
  
  colnames(prob.mat) <- 
    c("Naive", "Centroblast", "Centrocyte", "Memory", "Plasmablast")
  
  rownames(prob.mat) <- colnames(x)
  prob.mat[,1] <- t(x) %*% (as.matrix(BAGS.coef)[, "Naive"]) 
  prob.mat[,2] <- t(x) %*% (as.matrix(BAGS.coef)[, "Centroblast"]) 
  prob.mat[,3] <- t(x) %*% (as.matrix(BAGS.coef)[, "Centrocyte"]) 
  prob.mat[,4] <- t(x) %*% (as.matrix(BAGS.coef)[, "Memory"]) 
  prob.mat[,5] <- t(x) %*% (as.matrix(BAGS.coef)[, "Plasmablast"]) 
  
  prob.mat <- exp(prob.mat)
  return(prob.mat/apply(prob.mat, 1, sum))  # use rowSums ??
}