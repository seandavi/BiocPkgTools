context("biocBuildReport")
library(BiocPkgTools)

# test release build report
test_that("columns are consistent in report output", {
    columnames <- c("pkg", "author", "version", "git_last_commit",
        "git_last_commit_date", "node", "stage")

    bioc3.12_build <- biocBuildReport("3.12")
    expect_true(all(columnames %in% colnames(bioc3.12_build)))
    expect_true(inherits(bioc3.12_build[["git_last_commit_date"]], "POSIXct"))

    bioc3.13_build <- biocBuildReport("3.13")
    expect_true(all(columnames %in% colnames(bioc3.13_build)))
    expect_true(inherits(bioc3.13_build[["git_last_commit_date"]], "POSIXct"))

    biocdevel_build <- biocBuildReport("3.14")
    expect_true(all(columnames %in% colnames(biocdevel_build)))
    expect_true(inherits(biocdevel_build[["git_last_commit_date"]], "POSIXct"))
})

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
