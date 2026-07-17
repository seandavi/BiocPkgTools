test_that("biocPkgRanges returns a split list of flagged packages", {
    skip_if_bioc_offline()

    ## Narrow alphabetical range to keep the test fast; BUILD_STATUS_DB is
    ## downloaded once per Bioconductor version and cached.
    res <- biocPkgRanges(
        start = "a", end = "b", condition = "ERROR", version = "release"
    )

    expect_type(res, "list")
    expect_true(length(res) > 0)
    ## split() by pkg name means every element is a data.frame named after
    ## a package within the [a, b] range
    expect_true(all(nzchar(names(res))))
    for (pkgname in names(res)) {
        expect_true(pkgname >= "a" && pkgname <= "b")
        expect_s3_class(res[[pkgname]], "data.frame")
        expect_true(all(grepl("ERROR", res[[pkgname]][["result"]])))
    }
})

test_that("biocPkgRanges errors when no packages fall in range", {
    skip_if_bioc_offline()

    expect_error(
        biocPkgRanges(
            start = "zzzzzzzzz1", end = "zzzzzzzzz2", version = "release"
        ),
        "no packages in range"
    )
})

test_that("biocPkgRanges validates 'condition' and 'version' arguments", {
    skip_if_bioc_offline()

    expect_error(
        biocPkgRanges(start = "a", end = "b", condition = "NOTREAL"),
        "'arg' should be one of"
    )
    expect_error(
        biocPkgRanges(start = "a", end = "b", version = "NOTREAL"),
        "'arg' should be one of"
    )
})
