context("biocBuildReport")
library(BiocPkgTools)

bioc3.5_build = biocBuildReport("3.5")
bioc3.12_build = biocBuildReport(version = "3.12")


test_that("catch non-character version parameter ", {
    expect_error(biocBuildReport(2.3))
})

test_that("biocBuildReport returns appropriate class", {
    expect_true(tibble::is_tibble(bioc3.5_build))
    expect_true(tibble::is_tibble(bioc3.12_build))
})

test_that("nrow is correct", {
  expect_equal(nrow(bioc3.5_build), 15003)
  # This test fails when the build system changes
  # expect_equal(nrow(bioc3.12_build), 27466)
})

test_that("ncol is correct", {
  expect_equal(ncol(bioc3.5_build), 9)
  expect_equal(ncol(bioc3.12_build), 9)
})
