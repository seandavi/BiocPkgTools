context("getData")

## ---------------------------------------------------------------------
## pure data-wrangling helpers (no network required)
## ---------------------------------------------------------------------

test_that("author_list_to_string collapses author lists correctly", {
    res <- BiocPkgTools:::author_list_to_string(
        list("Alice", "Alice and Bob", c("Alice", "Bob", "Carol"))
    )
    expect_equal(
        res,
        c("Alice", "Alice and Bob", "Alice, Bob and Carol")
    )
})

test_that("summarise_dl_stats computes month and total downloads", {
    dl <- data.frame(
        Package = c("pkgA", "pkgA", "pkgB"),
        Nb_of_downloads = c(10, 20, 5)
    )
    res <- BiocPkgTools:::summarise_dl_stats(dl)

    expect_s3_class(res, "tbl_df")
    expect_equal(res[["Package"]], c("pkgA", "pkgB"))
    ## downloads_month is the *first* value encountered per package
    expect_equal(res[["downloads_month"]], c(10, 5))
    expect_equal(res[["downloads_total"]], c(30, 5))
})

test_that("process_data joins package metadata with download stats", {
    pkg_list <- data.frame(
        Author = I(list("Alice", "Bob and Carol")),
        Package = c("pkgA", "pkgB"),
        License = c("GPL-2", "MIT"),
        biocViews = c("Software", "Annotation"),
        Description = c("desc a", "desc b"),
        stringsAsFactors = FALSE
    )
    raw_dl <- data.frame(
        Package = c("pkgA", "pkgA", "pkgB"),
        Nb_of_downloads = c(10, 20, 5)
    )

    res <- BiocPkgTools:::process_data(pkg_list, raw_dl)

    expect_equal(
        colnames(res),
        c(
            "authors", "name", "license", "tags", "description",
            "downloads_month", "downloads_total", "page"
        )
    )
    expect_equal(res[["name"]], c("pkgA", "pkgB"))
    expect_equal(res[["authors"]], c("Alice", "Bob and Carol"))
    expect_equal(res[["downloads_total"]], c(30, 5))
    expect_true(all(grepl(
        "^https://bioconductor.org/packages/release/bioc/html/",
        res[["page"]]
    )))
})

test_that("process_data drops packages absent from the download stats", {
    pkg_list <- data.frame(
        Author = I(list("Alice")),
        Package = c("pkgOnlyInList"),
        License = "GPL-2",
        biocViews = "Software",
        Description = "desc",
        stringsAsFactors = FALSE
    )
    raw_dl <- data.frame(
        Package = "pkgSomethingElse",
        Nb_of_downloads = 1
    )
    res <- BiocPkgTools:::process_data(pkg_list, raw_dl)
    expect_equal(nrow(res), 0L)
})

## ---------------------------------------------------------------------
## BiocExplorer-cache helpers
##
## These all key off `system.file(..., package = "BiocExplorer")`, a
## *different*, optional package that is not a dependency of BiocPkgTools
## and is not installed in this environment (nor on CRAN/Bioconductor as
## a hard dependency). `system.file()` returns "" when a package isn't
## installed, so these functions exercise their real, documented
## "no cache available" behaviour without any mocking.
## ---------------------------------------------------------------------

test_that("has_cached_data reports FALSE when BiocExplorer isn't installed", {
    skip_if_not(
        !nzchar(system.file(package = "BiocExplorer")),
        "BiocExplorer is installed in this environment"
    )
    expect_false(BiocPkgTools:::has_cached_data())
})

test_that("cache_last_update_days returns NA when there is no cache", {
    skip_if_not(
        !nzchar(system.file(package = "BiocExplorer")),
        "BiocExplorer is installed in this environment"
    )
    expect_true(is.na(suppressWarnings(
        BiocPkgTools:::cache_last_update_days()
    )))
})

test_that("get_cached_data errors when there is no cache to read", {
    skip_if_not(
        !nzchar(system.file(package = "BiocExplorer")),
        "BiocExplorer is installed in this environment"
    )
    expect_error(suppressWarnings(BiocPkgTools:::get_cached_data()))
})

test_that("write_to_cache errors when the cache location is unavailable", {
    skip_if_not(
        !nzchar(system.file(package = "BiocExplorer")),
        "BiocExplorer is installed in this environment"
    )
    expect_error(suppressWarnings(BiocPkgTools:::write_to_cache("{}")))
})

## ---------------------------------------------------------------------
## get_bioc_data() -- full, real pipeline
## ---------------------------------------------------------------------

test_that("get_bioc_data downloads and assembles a package-data JSON blob", {
    skip_if_bioc_offline()

    json <- get_bioc_data()
    expect_type(json, "character")
    expect_s3_class(json, "json")

    df <- jsonlite::fromJSON(json)
    expect_true(all(c(
        "authors", "name", "license", "tags", "description",
        "downloads_month", "downloads_total", "page"
    ) %in% colnames(df)))
    expect_gt(nrow(df), 0L)
    ## rows with no biocViews tag are filtered out
    expect_false(any(is.na(df[["tags"]])))
})
