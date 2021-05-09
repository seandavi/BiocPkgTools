context("biocBuildReport")
library(BiocPkgTools)

# test release build report
bioc3.12_build <- biocBuildReport("3.12")

test_that("biocBuildReport returns appropriate class", {
    expect_true(tibble::is_tibble(bioc3.12_build))
})

test_that("nrow is correct", {
  # This test fails when the build system changes
  # expect_equal(nrow(bioc3.12_build), 27466)
})

test_that("ncol is correct", {
  expect_equal(ncol(bioc3.12_build), 9)
})
