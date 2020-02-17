context("getPackageInfo")
library(BiocPkgTools)

test_that("getPackageInfo", {
    expect_error(getPackageInfo("../test"),
                 'dir "../test" does not exist')
    expect_error(getPackageInfo("../"),
                 'dir "../" does not contain')
    actual <- getPackageInfo(system.file(package = "BiocManager"))
    expect_true(is.matrix(actual))
    expect_type(actual,"character")
    expect_equal(colnames(actual),c("Package","Author"))
})
