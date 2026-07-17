context("biocDownloadStats")

## ---------------------------------------------------------------------
## biocDownloadStats()
## ---------------------------------------------------------------------

test_that("biocDownloadStats returns tidy download stats for a small repo", {
    skip_if_bioc_offline()

    ## "workflows" is by far the smallest repo type, keeping this test fast
    tbl <- biocDownloadStats(pkgType = "workflows")

    expect_s3_class(tbl, "bioc_downloads")
    expect_s3_class(tbl, "tbl_df")
    expect_true(all(c(
        "pkgType", "Package", "Year", "Month",
        "Nb_of_distinct_IPs", "Nb_of_downloads", "Date"
    ) %in% colnames(tbl)))
    expect_gt(nrow(tbl), 0L)
    expect_true(all(tbl[["pkgType"]] == "workflows"))
    expect_s3_class(tbl[["Date"]], "Date")
    ## the "all" month summary row should have been filtered out
    expect_false("all" %in% tbl[["Month"]])
})

test_that("biocDownloadStats errors on an invalid pkgType", {
    expect_error(
        biocDownloadStats(pkgType = "not-a-real-type"),
        "should be one of"
    )
})

## ---------------------------------------------------------------------
## pkgDownloadStats()
## ---------------------------------------------------------------------

test_that("pkgDownloadStats returns monthly stats for one package/year", {
    skip_if_bioc_offline()

    tbl <- pkgDownloadStats("S4Vectors", years = 2024)

    expect_s3_class(tbl, "tbl_df")
    expect_true(all(c(
        "Year", "Month", "Nb_of_distinct_IPs", "Nb_of_downloads"
    ) %in% colnames(tbl)))
    expect_true(all(tbl[["Year"]] == 2024))
    expect_gte(nrow(tbl), 10L)
    ## rows with zero counts are filtered out
    expect_true(all(tbl[["Nb_of_distinct_IPs"]] != 0))
    expect_true(all(tbl[["Nb_of_downloads"]] != 0))
})

test_that("pkgDownloadStats handles a non-existent package gracefully", {
    skip_if_bioc_offline()

    expect_warning(
        res <- pkgDownloadStats(
            "ThisPackageDoesNotExistXYZ123", years = 2024
        ),
        "No data for year"
    )
    expect_s3_class(res, "tbl_df")
    expect_equal(nrow(res), 0L)
})

test_that("pkgDownloadStats errors on invalid pkgType", {
    expect_error(
        pkgDownloadStats("S4Vectors", pkgType = "not-a-real-type"),
        "should be one of"
    )
})

## ---------------------------------------------------------------------
## firstInBioc()
## ---------------------------------------------------------------------

test_that("firstInBioc reports the earliest month per package", {
    skip_if_bioc_offline()

    dls <- biocDownloadStats(pkgType = "workflows")
    first <- firstInBioc(dls)

    expect_s3_class(first, "tbl_df")
    ## one row per distinct package
    expect_equal(nrow(first), length(unique(dls[["Package"]])))

    ## for a sample package, the reported date should be that package's
    ## earliest date in the full table
    pkg <- first[["Package"]][[1]]
    expected_min <- min(dls[["Date"]][dls[["Package"]] == pkg])
    expect_equal(first[["Date"]][first[["Package"]] == pkg], expected_min)
})

test_that("firstInBioc handles an empty download-stats table", {
    empty <- dplyr::tibble(
        Package = character(0L), Month = character(0L),
        Date = as.Date(character(0L))
    )
    res <- firstInBioc(empty)
    expect_equal(nrow(res), 0L)
})

## ---------------------------------------------------------------------
## pkgDownloadRank()
## ---------------------------------------------------------------------

test_that("pkgDownloadRank returns a percentile rank with rank/total names", {
    skip_if_bioc_offline()

    pct <- pkgDownloadRank("S4Vectors", "software")

    expect_type(pct, "double")
    expect_gte(pct, 0)
    expect_lte(pct, 100)
    expect_match(names(pct), "^[0-9]+/[0-9]+$")
})

test_that("pkgDownloadRank errors on invalid pkgType", {
    expect_error(
        pkgDownloadRank("S4Vectors", pkgType = "not-a-real-type"),
        "should be one of"
    )
})

## ---------------------------------------------------------------------
## latestPkgStats()
## ---------------------------------------------------------------------

test_that("latestPkgStats combines download, rank and activity stats", {
    skip_if_offline()
    skip_if_bioc_offline()

    res <- latestPkgStats(
        "Bioconductor/S4Vectors", Sys.Date() - 20, pkgType = "software"
    )

    expect_s3_class(res, "tbl_df")
    expect_equal(nrow(res), 1L)
    expect_true(all(c(
        "Package", "Date", "avg_mo_distict_IPs", "avg_mo_downloads",
        "downloadRank", "issuesClosedSince", "commitsSince"
    ) %in% colnames(res)))
    expect_equal(res[["Package"]], "S4Vectors")
})

## ---------------------------------------------------------------------
## activitySince()
## ---------------------------------------------------------------------

test_that("activitySince retrieves recently closed issues", {
    skip_if_offline()

    res <- activitySince(
        "seandavi/BiocPkgTools", "issues", "closed",
        Date = format(Sys.Date() - 30)
    )

    expect_s3_class(res, "tbl_df")
    expect_true(all(c("created_at", "number", "title") %in% colnames(res)))
    expect_gt(nrow(res), 0L)
})

test_that("activitySince retrieves recent commits", {
    skip_if_offline()

    res <- activitySince(
        "seandavi/BiocPkgTools", "commits", Date = format(Sys.Date() - 10)
    )

    expect_s3_class(res, "tbl_df")
    expect_true(all(c(
        "committer_date", "commit", "parents", "author", "message"
    ) %in% colnames(res)))
    expect_gt(nrow(res), 0L)
})

test_that("activitySince errors on an invalid activity type", {
    expect_error(
        activitySince("seandavi/BiocPkgTools", "bogus", Date = "2024-01-01"),
        "should be one of"
    )
})

test_that("activitySince errors for a non-existent repository", {
    skip_if_offline()

    expect_error(
        activitySince(
            "seandavi/ThisRepoDoesNotExist12345XYZ", "issues", "closed",
            Date = format(Sys.Date() - 10)
        )
    )
})

## ---------------------------------------------------------------------
## internal helpers
## ---------------------------------------------------------------------

test_that(".cache_read reads a single package stats file", {
    skip_if_bioc_offline()

    df <- BiocPkgTools:::.cache_read(
        "https://bioconductor.org/packages/stats/bioc/S4Vectors/S4Vectors_stats.tab"
    )
    expect_s3_class(df, "data.frame")
    expect_true(all(c(
        "Year", "Month", "Nb_of_distinct_IPs", "Nb_of_downloads"
    ) %in% colnames(df)))
    expect_gt(nrow(df), 0L)
})

test_that(".get_all_biocpkgs lists current Bioconductor package names", {
    skip_if_bioc_offline()

    pkgs <- BiocPkgTools:::.get_all_biocpkgs()
    expect_type(pkgs, "character")
    expect_gt(length(pkgs), 1000L)
    expect_true("S4Vectors" %in% pkgs)
})

test_that(".filter_http_error keeps reachable and drops unreachable URLs", {
    skip_if_bioc_offline()

    urls <- c(
        good = "https://bioconductor.org",
        bad = "https://invalid.url.example.nonexistent12345.com"
    )
    expect_warning(
        res <- BiocPkgTools:::.filter_http_error(urls),
        "currently down"
    )
    expect_equal(names(res), "good")
    expect_length(res, 1L)
})

test_that(".gh_data_process dispatches to .commit_table for commits", {
    jsonlist <- list(
        parents = list(list(sha = "parentsha1")),
        commit = list(
            author = list(name = "Jane Doe"),
            tree = list(sha = "treesha123"),
            committer = list(date = "2024-01-01T00:00:00Z"),
            message = "Initial commit"
        )
    )
    res <- BiocPkgTools:::.gh_data_process(jsonlist, fields = NULL, commits = TRUE)

    expect_s3_class(res, "tbl_df")
    expect_equal(res[["author"]], "Jane Doe")
    expect_equal(res[["commit"]], "treesha123")
    expect_equal(res[["message"]], "Initial commit")
    expect_true(is.na(res[["parents"]]))
})

test_that(".gh_data_process subsets fields for non-commit activity", {
    x <- list(
        created_at = "2024-01-01", number = 5L, title = "Some issue",
        extra = "ignore me"
    )
    res <- BiocPkgTools:::.gh_data_process(
        x, fields = c("created_at", "number", "title"), commits = FALSE
    )
    expect_equal(names(res), c("created_at", "number", "title"))
    expect_null(res[["extra"]])
})

test_that(".commit_table builds a one-row tibble from a commit JSON list", {
    jsonlist <- list(
        parents = NULL,
        commit = list(
            author = list(name = "Alice"),
            tree = list(sha = "abc123"),
            committer = list(date = "2023-05-01T00:00:00Z"),
            message = "Fix bug"
        )
    )
    res <- BiocPkgTools:::.commit_table(jsonlist)

    expect_s3_class(res, "tbl_df")
    expect_equal(nrow(res), 1L)
    expect_equal(res[["author"]], "Alice")
    expect_equal(res[["commit"]], "abc123")
    expect_true(is.na(res[["parents"]]))
    expect_equal(res[["message"]], "Fix bug")
})

test_that(".try.read.table reads real data and handles missing years", {
    skip_if_bioc_offline()

    ok <- BiocPkgTools:::.try.read.table(
        "https://bioconductor.org/packages/stats/bioc/S4Vectors/S4Vectors_2024_stats.tab",
        header = TRUE
    )
    expect_s3_class(ok, "data.frame")
    expect_gt(nrow(ok), 0L)

    expect_warning(
        empty <- BiocPkgTools:::.try.read.table(
            "https://bioconductor.org/packages/stats/bioc/NoSuchPkgXYZ/NoSuchPkgXYZ_2024_stats.tab",
            header = TRUE
        ),
        "No data for year"
    )
    expect_s3_class(empty, "data.frame")
    expect_equal(nrow(empty), 0L)
})
