context("biocMaintained")

test_that("biocMaintained returns packages maintained by the Bioconductor core team", {
    skip_if_bioc_offline()

    bm <- biocMaintained()

    expect_s3_class(bm, "data.frame")
    expect_gt(nrow(bm), 0L)
    expect_true("Package" %in% colnames(bm))
    expect_true("Maintainer" %in% colnames(bm))
    expect_true(all(
        grepl("maintainer@bioconductor.org", bm[["Maintainer"]], ignore.case = TRUE)
    ))
    expect_identical(attr(bm, "maintainer"), "maintainer@bioconductor.org")
    expect_identical(
        attr(bm, "pkgType"),
        c("software", "data-experiment", "workflows", "data-annotation")
    )
})

test_that("biocMaintained restricts results to the requested pkgType", {
    skip_if_bioc_offline()

    bm_software <- biocMaintained(pkgType = "software")
    expect_identical(attr(bm_software, "pkgType"), "software")
    expect_gt(nrow(bm_software), 0L)
})

test_that("biocMaintained errors on an invalid pkgType", {
    expect_error(
        biocMaintained(pkgType = "not-a-real-pkgtype"),
        "'arg' should be one of"
    )
})

test_that("hasBiocMaint correctly classifies core-team vs non-core maintainers", {
    skip_if_bioc_offline()

    # "annotate" is maintained by maintainer@bioconductor.org; "limma" is
    # maintained by an individual. A nonexistent package name should be
    # handled gracefully (FALSE, not an error/NA) per the documented
    # behavior of falling back to the input name when unmatched.
    res <- hasBiocMaint(c("annotate", "limma", "NotARealPackageXYZ123"))

    expect_type(res, "logical")
    expect_identical(names(res), c("annotate", "limma", "NotARealPackageXYZ123"))
    expect_true(res[["annotate"]])
    expect_false(res[["limma"]])
    expect_false(res[["NotARealPackageXYZ123"]])
})

test_that("hasBiocMaint handles a single package name", {
    skip_if_bioc_offline()

    res <- hasBiocMaint("BiocGenerics")
    expect_type(res, "logical")
    expect_length(res, 1L)
    expect_identical(names(res), "BiocGenerics")
})
