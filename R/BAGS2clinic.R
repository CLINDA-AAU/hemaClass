#' Classification of cell of origin with Nanostring data
#' 
#' Classification using the B-cell associated gene signatures (BAGS) classifier 
#' of Dybkaer et al. [1] adjusted for nanostring data by Michaelsen et al. [2].
#' The BAGS classifier attemps to assign the given 
#' supplied Diffuse Large B-Cell Lymphomas (DLBCL) 
#' samples to one of four classes according to their resembalace to normal 
#' Centrocytes (CC), Centroblasts (CB), Memory (M), and 
#' Plasmablasts (PB) cells. 
#' 
#' @rdname BAGS2Clinic
#' @aliases 
#'   BAGS2Clinic
#'   BAGS2ClinicClassifier
#'   BAGS2Clinicclassifier 
#' @param new.data An expression \code{matrix} containing the normalized data, 
#' with samples in columns and genes in rows. Rownames should be genenames in 
#' hgnc nomenclature.
#' @param cut.spec A \code{numeric} value used to threshold the probabilities 
#'   and determine the class.
#' @param percent.classified A \code{numeric} value indicating the percentage of 
#'   patients that should be classified. An alternative to \code{cut.spec}.
#' @return A \code{list} of probabilities regarding each patients association 
#'   with each class, the determined class, and the used cut-off thresholds.
#' @details The function BAGS2Clinic classifies DLBCL patients according to the cell 
#'   of origin for the tumor [1,2].
#' @references
#' [1] Dybkaer, Karen, Martin Boegsted, Steffen Falgreen, Julie S. Boedker, 
#' Malene K. Kjeldsen, Alexander Schmitz, Anders E. Bilgrau et al. 
#' "Diffuse large B-cell lymphoma classification system that associates 
#' normal B-cell subset phenotypes with prognosis." 
#' Journal of Clinical Oncology 33, no. 12 (2015): 1379-1388.
#' 
#' [2] Michaelsen, T. Y. et al. "A B-cell-associated gene signature
#'  classification of diffuse large B-cell lymphoma by NanoString
#'   technology." Blood advances, 2(13) (2018): 1542-1546.
#'
#' @author 
#'   Thomas Yssing Michaelsen <tym (at) bio.aau.dk> \cr 
#'   Rasmus Broendum <rfb (at) rn.dk>
#' @examples
#' \donttest{
#' # Nanostring files may be read and normalized using the NanoStringNorm package
#' data.nano.raw <- read.markup.RCC("path to files")
#'
#' data.nano.norm <- NanoStringNorm(data.nano.raw, 
#'                                  Background = "mean",
#'                                  CodeCount = "sum",
#'                                  return.matrix.of.endogenous.probes = FALSE)
#' 
#' # Data should be log2 transformed and scaled
#' data.nano.sc <- microarrayScale(log2(as.matrix(data.nano.norm$normalized.data[,-c(1,2,3)]) + 1))) 
#'  
#' # We may now use the BAGS classifier
#' BAGS2Clinic(data.nano.sc)
#' }                                                                                                                  
#' @export
# Function to generate BAGS2Clinic labels.
BAGS2Clinic <- function(new.data, cut.spec = NULL, percent.classified = 85){
  
  new.data[is.na(new.data)] <- 0
  
  train.mat <- BAGS2ClinicProb(new.data)
  
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
                  levels = c("Centroblast", "Centrocyte", 
                             "Memory", "Plasmablast", "Unclassified"))
  
  return(list(class = class, prob.mat = train.mat, prob = prob, cut = cut))
}

# Function to generate probabilities.
BAGS2ClinicProb <- function(newx){
  BAGS.coef <- readBAGS2clinicCoef()
  
  diff <- setdiff(row.names(BAGS.coef)[-1], rownames(newx))
  
  if (length(diff)) {
    missing <-  matrix(0, ncol = ncol(newx), nrow = length(diff), 
                       dimnames = list(diff, colnames(newx)))
    
    newx <- rbind(newx, missing)
    
    warning("The following probesets are missing:\\n", 
            paste(diff, collapse = ", "))
  }
  
  x <- rbind(1, newx[row.names(BAGS.coef)[-1], , drop = FALSE])
  
  prob.mat <- matrix(ncol = 4, nrow = ncol(x))
  
  colnames(prob.mat) <- 
    c("Centroblast", "Centrocyte", "Memory", "Plasmablast")
  
  rownames(prob.mat) <- colnames(x)
  prob.mat[,1] <- t(x) %*% (as.matrix(BAGS.coef)[, "Centroblast"]) 
  prob.mat[,2] <- t(x) %*% (as.matrix(BAGS.coef)[, "Centrocyte"]) 
  prob.mat[,3] <- t(x) %*% (as.matrix(BAGS.coef)[, "Memory"]) 
  prob.mat[,4] <- t(x) %*% (as.matrix(BAGS.coef)[, "Plasmablast"]) 
  
  prob.mat <- exp(prob.mat)
  return(prob.mat/apply(prob.mat, 1, sum))
}

#readRCC <- function(...){
#  dat <- read.markup.RCC(...)
#  
#  # Expression matrix.
#  x <- dat$x %>% .[.$CodeClass == "Endogenous",] %>% 
#    `rownames<-`(.,.$Name) %>% 
#    dplyr::select(-Name,-Accession,-CodeClass) %>% as.matrix()
#  
#  # Metadata.
#  meta  <- dat$header %>% t %>% data.frame(.,stringsAsFactors = F)
#  desc  <- data.frame(
#    labelDescription = colnames(meta),
#    row.names        = colnames(meta))
#  phdat <- new("AnnotatedDataFrame",
#               data        = meta,
#               varMetadata = desc,
#               dimLabels   = c("sampleNames","sampleColumns"))
#  
#  # Assemble.
#  ExpressionSet(assayData = x,phenoData = phdat)
#}