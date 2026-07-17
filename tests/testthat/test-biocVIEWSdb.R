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

test_that(".read_VIEWS_url parses a real VIEWS file into a data.frame", {
    skip_if_bioc_offline()

    version <- as.package_version(BiocManager:::.version_bioc("devel"))
    url <- BiocPkgTools:::.get_VIEWS_url(version = version, type = "BioCsoft")

    df <- BiocPkgTools:::.read_VIEWS_url(url)

    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0L)
    expect_true(all(c("Package", "Version", "Maintainer") %in% colnames(df)))
})

test_that(".read_VIEWS_url errors informatively when the resource isn't DCF-formatted", {
    skip_if_bioc_offline()

    # A real, fetchable Bioconductor URL that is not a VIEWS/DCF file --
    # exercises .read_VIEWS_url()'s own error-wrapping branch (read.dcf()
    # failing on non-DCF content) rather than a network failure.
    expect_error(
        BiocPkgTools:::.read_VIEWS_url("https://bioconductor.org/index.html"),
        "Error reading VIEWS file from URL"
    )
})
