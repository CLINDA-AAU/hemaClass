
# Build the classifier
require(survival)

metadataLLMPPRCHOP <- 
  readRDS("inst/website/Database/LLMPPRCHOP/metadataLLMPPRCHOP.rds")

GEPLLMPPRCHOP <- 
  readRDS("inst/website/Database/LLMPPRCHOP/GEPLLMPPRCHOP_RMA_affy.rds")

GEPLLMPPRCHOP.sc          <- microarrayScale(exprs(GEPLLMPPRCHOP))
metadataLLMPPRCHOP$BAGS   <- BAGS(GEPLLMPPRCHOP.sc)$class
metadataLLMPPRCHOP$ABCGCB <- ABCGCB(GEPLLMPPRCHOP.sc)$class

fit <- coxph(OS ~ BAGS * ABCGCB + ipi, data = metadataLLMPPRCHOP)  

