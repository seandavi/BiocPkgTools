## getYearsInBioc.R functions can, in the worst case (no cache present),
## scan every Bioconductor release ever published -- this is far too slow
## for a unit test. To keep these tests fast (seconds, not minutes) we:
##  * test `.formatBiocYearsDF()` entirely offline with a tiny fabricated
##    two/three-release manifest (no network at all);
##  * test `.updateYearsInBioc()` against a manifest that already covers
##    every currently-published release, so the "needed releases" diff is
##    empty and no per-release package scan is triggered (one small
##    config.yaml fetch only);
##  * test `.genBiocManifestDF()` for exactly ONE Bioconductor version (not a
##    range), which is the narrowest possible live slice of that helper;
##  * test `getPkgYearsInBioc()` end-to-end by pre-seeding an isolated
##    BiocFileCache cache with a tiny fabricated manifest (two fake package
##    names, real release/date config) so the "cache exists" branch is
##    taken and only one live config.yaml fetch happens (via
##    `.updateYearsInBioc()`), never a real per-release package scan.

test_that(".formatBiocYearsDF computes first/last-seen years from a tiny fabricated manifest", {
    config <- list(
        release_dates = list(`3.20` = "10/30/2024", `3.21` = "4/9/2025"),
        devel_version = "3.22"
    )
    manifestDF <- data.frame(
        Release = c("3.20", "3.21", "3.22"),
        major = c(3, 3, 3),
        minor = c(20, 21, 22)
    )
    manifestDF$packages <- list(
        tibble::tibble(
            category = c("bioc", "bioc"), package = c("PkgA", "PkgB")
        ),
        tibble::tibble(category = "bioc", package = "PkgA"),
        tibble::tibble(category = "bioc", package = "PkgA")
    )
    NeededYearsData <- list(config = config, manifestDF = manifestDF)

    result <- BiocPkgTools:::.formatBiocYearsDF(NeededYearsData)

    expect_setequal(result[["package"]], c("PkgA", "PkgB"))

    pkgA <- result[result[["package"]] == "PkgA", ]
    expect_identical(pkgA[["first_version_available"]], "3.20")
    ## PkgA is present in the devel release => still active, not removed
    expect_true(is.na(pkgA[["last_version_available"]]))
    expect_false(is.na(pkgA[["approx_years_in"]]))
    expect_true(is.na(pkgA[["years_before_rm"]]))

    pkgB <- result[result[["package"]] == "PkgB", ]
    expect_identical(pkgB[["first_version_available"]], "3.20")
    ## PkgB only ever appeared in 3.20 => removed before devel
    expect_identical(pkgB[["last_version_available"]], "3.20")
    expect_true(is.na(pkgB[["approx_years_in"]]))
    expect_false(is.na(pkgB[["years_before_rm"]]))
})

test_that(".updateYearsInBioc is a no-op when the manifest is already current", {
    skip_if_bioc_offline()

    current_config <- yaml::read_yaml("https://bioconductor.org/config.yaml")
    current_bioc_ver <- names(current_config[["r_ver_for_bioc_ver"]])
    ## 1.6/1.7 are deliberately excluded by the implementation (no data)
    all_known_releases <- setdiff(current_bioc_ver, c("1.6", "1.7"))

    manifestDF <- data.frame(Release = all_known_releases)
    NeededYearsData <- list(
        config = list(devel_version = "unused"), manifestDF = manifestDF
    )

    result <- BiocPkgTools:::.updateYearsInBioc(NeededYearsData, tempfile())

    ## no releases were "needed", so the object is returned unchanged and no
    ## per-release package scan occurred (this keeps the test fast)
    expect_identical(result, NeededYearsData)
})

test_that(".genBiocManifestDF fetches the package manifest for a single release", {
    skip_if_bioc_offline()

    ## Deliberately test a single, narrow, historical version rather than a
    ## live "current" version, so the test result is stable over time.
    result <- BiocPkgTools:::.genBiocManifestDF(3, 21)

    expect_s3_class(result, "tbl_df")
    expect_setequal(
        colnames(result), c("category", "package")
    )
    expect_true("bioc" %in% result[["category"]])
    expect_true("BiocPkgTools" %in% result[["package"]])
})

test_that("getPkgYearsInBioc filters a pre-seeded, tiny fabricated cache", {
    skip_if_bioc_offline()

    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    current_config <- yaml::read_yaml("https://bioconductor.org/config.yaml")
    current_bioc_ver <- names(current_config[["r_ver_for_bioc_ver"]])
    all_known_releases <- setdiff(current_bioc_ver, c("1.6", "1.7"))

    manifestDF <- data.frame(Release = all_known_releases)
    manifestDF$major <- as.integer(sub("\\..*", "", manifestDF$Release))
    manifestDF$minor <- as.integer(sub(".*\\.", "", manifestDF$Release))
    ## Fabricate a tiny two-package manifest: FakePkgOne present in every
    ## release (still "active"), FakePkgTwo present only in the oldest
    ## release (i.e., "removed").
    manifestDF$packages <- lapply(seq_len(nrow(manifestDF)), function(i) {
        if (i == 1) {
            tibble::tibble(
                category = c("bioc", "bioc"),
                package = c("FakePkgOne", "FakePkgTwo")
            )
        } else {
            tibble::tibble(category = "bioc", package = "FakePkgOne")
        }
    })
    NeededYearsData <- list(config = current_config, manifestDF = manifestDF)

    bfc <- BiocPkgTools:::.get_cache()
    pkginfo <- BiocFileCache::bfcnew(bfc, "bioc.pkg.years.info", ext = ".Rdata")
    save(NeededYearsData, file = pkginfo)

    result <- getPkgYearsInBioc(c("FakePkgOne", "FakePkgTwo"))

    expect_s3_class(result, "data.frame")
    expect_setequal(result[["package"]], c("FakePkgOne", "FakePkgTwo"))

    one <- result[result[["package"]] == "FakePkgOne", ]
    expect_true(is.na(one[["last_version_available"]]))

    two <- result[result[["package"]] == "FakePkgTwo", ]
    expect_false(is.na(two[["last_version_available"]]))
    expect_false(is.na(two[["years_before_rm"]]))
})
