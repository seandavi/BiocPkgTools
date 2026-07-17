context("biocBuildStatusDB")

test_that("biocBuildStatusDB returns tidy build status data for devel", {
    skip_if_bioc_offline()

    db <- biocBuildStatusDB("devel")

    expect_s3_class(db, "data.frame")
    expect_identical(colnames(db), c("pkg", "node", "stage", "result"))
    expect_gt(nrow(db), 0L)
    expect_false(is.null(attr(db, "BioCversion")))
    expect_false(is.null(attr(db, "retrieved")))
})

test_that("biocBuildStatusDB forces pkgType to 'software' for versions < 3.14", {
    skip_if_bioc_offline()

    # Versions before 3.14 only ever published a software build status db;
    # requesting all four pkgTypes should silently fall back to software
    # only rather than erroring on the other (nonexistent) status files.
    db_old <- biocBuildStatusDB("3.12")

    expect_s3_class(db_old, "data.frame")
    expect_gt(nrow(db_old), 0L)
    expect_identical(colnames(db_old), c("pkg", "node", "stage", "result"))
})

test_that("biocBuildStatusDB restricts results to a single requested pkgType", {
    skip_if_bioc_offline()

    db <- biocBuildStatusDB("devel", pkgType = "workflows")
    expect_s3_class(db, "data.frame")
    expect_gt(nrow(db), 0L)
})

test_that("biocBuildStatusDB errors on an invalid version", {
    expect_error(
        biocBuildStatusDB(version = "not-a-version"),
        "'version' is not 'release', 'devel', or a valid 'package_version'"
    )
})

test_that("biocBuildStatusDB errors on an invalid pkgType", {
    expect_error(
        biocBuildStatusDB("devel", pkgType = "not-a-real-pkgtype"),
        "'arg' should be one of"
    )
})
