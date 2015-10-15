context("Test <margin><Statistic> type functions (rowSds, colVars, etc)")


x <- matrix(rnorm(30), 5, 6)


test_that("Test that <margin><Statistic> (e.g. rowSds) type functions works", {
  expect_that(rowSds(x),  equals(apply(x, 1, sd)))
  expect_that(rowVars(x), equals(apply(x, 1, var)))
  expect_that(colSds(x),  equals(apply(x, 2, sd)))
  expect_that(colVars(x), equals(apply(x, 2, var)))
})


test_that("Test <margin><Statistic> (e.g. rowSds) one degenerated data", {
  expect_that(rowSds(x[0, ]), equals(apply(x[0, ], 1, sd)))
  expect_that(rowSds(x[ ,0]), equals(apply(x[ ,0], 1, sd)))
  expect_that(rowVars(x[0, ]), equals(apply(x[0, ], 1, var)))
  expect_that(rowVars(x[ ,0]), equals(apply(x[ ,0], 1, var)))
  expect_that(colSds(x[0, ]), equals(apply(x[0, ], 2, sd)))
  expect_that(colSds(x[ ,0]), equals(apply(x[ ,0], 2, sd)))
  expect_that(colVars(x[0, ]), equals(apply(x[0, ], 2, var)))
  expect_that(colVars(x[ ,0]), equals(apply(x[ ,0], 2, var)))
})

