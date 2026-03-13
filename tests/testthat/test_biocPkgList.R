context("biocPkgList")
library(BiocPkgTools)

bpkgl = biocPkgList()


test_that("pkg list has correct classes", {
    expect_is(bpkgl, 'tbl_df')
    expect_is(bpkgl, 'tbl')
    expect_is(bpkgl, 'data.frame')
})

test_that("ncol is approximately correct", {
  expect_gt(ncol(bpkgl), 35)
})

test_that("nrow is approximately correct", {
  expect_gt(nrow(bpkgl), 1000)
})

test_that("fnd column is present and is a list", {
  expect_true("fnd" %in% colnames(bpkgl))
  expect_type(bpkgl[["fnd"]], "list")
})

