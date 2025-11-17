context("CRANstatus")

test_that("CRANstatus works", {
    ## test a package that is likely to be OK
    res <- CRANstatus("BiocManager", dry.run = TRUE)
    expect_type(res, "logical")
    expect_true(res)
})
