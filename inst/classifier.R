
# Build the classifier
require(survival)

metadataLLMPPRCHOP <- readRDS(file = "inst/homepage/Database/V1/LLMPPRCHOP/Metadata/metadataLLMPPRCHOP.RDSData")

GEPLLMPPRCHOP <- readRDS(file = "inst/homepage/Database/V1/LLMPPRCHOP/GEP/GEPLLMPPRCHOP_RMA_affy.RDSData")

GEPLLMPPRCHOP.sc          <- microarrayScale(exprs(GEPLLMPPRCHOP))
metadataLLMPPRCHOP$BAGS   <- BAGS(GEPLLMPPRCHOP.sc)$class
metadataLLMPPRCHOP$ABCGCB <- ABCGCB(GEPLLMPPRCHOP.sc)$class

fit <- coxph(OS ~ BAGS * ABCGCB + ipi, data = metadataLLMPPRCHOP)  

