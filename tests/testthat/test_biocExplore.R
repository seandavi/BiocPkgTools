test_that("biocExplore validates the 'top' argument without touching network", {
    expect_error(biocExplore(top = 0), "top must be >= 1")
    expect_error(biocExplore(top = -1), "top must be >= 1")
    expect_error(biocExplore(top = "many"), "top must be >= 1")
})

test_that("biocExplore builds an htmlwidget from live Bioconductor data", {
    skip_if_bioc_offline()

    ## Uses biocPkgList()/biocDownloadStats() under the hood (both cached by
    ## BiocFileCache after the first call), so this is exercised elsewhere
    ## too; keep 'top' small since it only affects widget data downstream.
    widget <- biocExplore(top = 10L)

    expect_s3_class(widget, "htmlwidget")
    expect_s3_class(widget, "bioc_explore")
    expect_identical(widget[["x"]][["data"]][["top"]], 10L)
    expect_true(!is.null(widget[["x"]][["data"]][["data"]]))
})
