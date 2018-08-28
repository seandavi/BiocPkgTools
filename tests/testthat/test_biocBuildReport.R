context("biocBuildReport")
library(BiocPkgTools)

bioc3.5_build = biocBuildReport("3.5")


test_that("catch non-character version parameter ", {
    expect_error(biocBuildReport(2.3))
})

test_that("nrow is correct", {
  expect_equal(nrow(bioc3.5_build), 14913)
})

test_that("ncol is correct", {
  expect_equal(ncol(bioc3.5_build), 9)
})
