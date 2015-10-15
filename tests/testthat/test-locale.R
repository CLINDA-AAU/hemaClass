context("Test issue #6, different results under different locales")
# Test regarding https://github.com/oncoclass/hemaClass/issues/6

# List files
files <- list.files(system.file("extdata/celfiles", package = "hemaClass"), 
                    full.names = TRUE)

# Create first under one locale
affy.batch1     <- readCelfiles(filenames = files) 
rma.batch1      <- rmaPreprocessing(affy.batch1)
rma.batch1.ref1 <- rmaReference(affy.batch1, rma.batch1)

# Change locale, then redo
Sys.setlocale("LC_COLLATE", "C") 
affy.batch2 <- readCelfiles(filenames = files)
rma.batch2 <- rmaPreprocessing(affy.batch2)
rma.batch2.ref1 <- rmaReference(affy.batch2, rma.batch1)
rma.batch2.ref2 <- rmaReference(affy.batch2, rma.batch2)


test_that("Equivalent rma batches under different locales", {
  # Reference objects
  expect_equal(rma.batch1$exprs, 
               rma.batch2$exprs[rownames(rma.batch1$exprs), ])
  expect_equal(rma.batch1$exprs.sc, 
               rma.batch2$exprs.sc[rownames(rma.batch1$exprs.sc), ])
  expect_equal(rma.batch1$exprs.sc.mean, 
               rma.batch2$exprs.sc.mean[rownames(rma.batch1$exprs.sc.mean), ])
  expect_equal(rma.batch1$quantile, 
               rma.batch2$quantile)
  expect_equal(rma.batch1$alpha, 
               rma.batch2$alpha[names(rma.batch1$alpha)])  
  expect_equal(rma.batch1$sd,
               rma.batch2$sd[names(rma.batch1$sd)])
  expect_equal(rma.batch1$median,
               rma.batch2$median[names(rma.batch1$median)])
  expect_equal(rma.batch1$mean,
               rma.batch2$mean[names(rma.batch1$mean)])
  
  # Reference normalized objects are equivalent
  expect_equal(rma.batch1.ref1$exprs, 
               rma.batch2.ref2$exprs[rownames(rma.batch1.ref1$exprs), ])
  expect_equal(rma.batch1.ref1$exprs.sc, 
               rma.batch2.ref1$exprs.sc[rownames(rma.batch2.ref1$exprs.sc), ])
})


test_that("Identical classification results under different locales", {
  bags11 <- BAGS(rma.batch1.ref1$exprs.sc)
  bags21 <- BAGS(rma.batch2.ref1$exprs.sc)
  bags22 <- BAGS(rma.batch2.ref2$exprs.sc)
  expect_equal(bags11, bags21)
  expect_equal(bags11, bags22) 
})
