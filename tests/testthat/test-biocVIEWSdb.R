test_that("biocVIEWSdb returns a data.frame with rows", {
    pkgTypes <- c("software", "data-experiment", "data-annotation", "workflows")
    for (type in pkgTypes) {
        df <- biocVIEWSdb(pkgType = type)
        expect_s3_class(df, "data.frame")
        expect_gt(nrow(df), 0L)
    }
})

test_that("biocVIEWSdb handles invalid pkgType", {
    expect_error(
        biocVIEWSdb(pkgType = "invalid-type"),
        "'arg' should be one of"
    )
})

test_that("biocVIEWSdb handles invalid version", {
    expect_error(
        biocVIEWSdb(version = "invalid-version"),
        "'version' is not 'release', 'devel', or a valid 'package_version'"
    )
})
