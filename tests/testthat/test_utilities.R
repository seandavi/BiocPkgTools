context("utilities")

test_that(".get_remote_size works", {
    skip_if_offline()
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"
    size <- BiocPkgTools:::.get_remote_size(url)
    expect_type(size, "double")
    expect_gt(size, 0)

    # Check invalid URL
    expect_identical(
        BiocPkgTools:::.get_remote_size(
            "https://invalid.url.example.com/file.tab"
        ),
        NA_integer_
    )
})

test_that(".cache_url_file works", {
    skip_if_offline()
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"
    path <- BiocPkgTools:::.cache_url_file(url)
    expect_true(file.exists(path))
})

test_that(".cache_url_file redownloads a stale cached file", {
    skip_if_bioc_offline()
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"

    # Prime the cache with a real download, then deliberately corrupt the
    # cached local file so its size no longer matches the remote resource.
    # .cache_url_file() uses the package-wide default BiocFileCache(), so
    # there is no way to inject an isolated cache instance here -- instead
    # we rely on .cache_url_file() itself being self-healing (it detects
    # the size mismatch via .needs_update() and redownloads) to restore the
    # real content by the end of this test, whether or not the
    # expectations below pass.
    path <- BiocPkgTools:::.cache_url_file(url)
    real_size <- file.size(path)
    on.exit(BiocPkgTools:::.cache_url_file(url), add = TRUE)

    writeLines("deliberately corrupted cache entry", path)
    expect_lt(file.size(path), real_size)

    healed_path <- BiocPkgTools:::.cache_url_file(url)
    expect_identical(file.size(healed_path), real_size)
})

test_that(".needs_update handles the uncached / up-to-date / stale cases", {
    skip_if_bioc_offline()
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"

    # Use an isolated, throwaway BiocFileCache so this test cannot affect
    # (or be affected by) the package's shared default cache.
    bfc <- BiocFileCache::BiocFileCache(tempfile(), ask = FALSE)

    # Not yet cached: bfcquery() returns 0 rows -> NA
    bquery_empty <- BiocFileCache::bfcquery(bfc, url, "rname", exact = TRUE)
    expect_identical(nrow(bquery_empty), 0L)
    expect_true(is.na(BiocPkgTools:::.needs_update(bfc, bquery_empty, url)))

    # Real download into the isolated cache as a proper 'web' resource
    BiocFileCache::bfcadd(
        bfc, rname = url, fpath = url, rtype = "web",
        action = "copy", download = TRUE
    )
    bquery <- BiocFileCache::bfcquery(bfc, url, "rname", exact = TRUE)
    expect_identical(nrow(bquery), 1L)

    # Freshly downloaded and size matches remote -> not stale
    expect_false(BiocPkgTools:::.needs_update(bfc, bquery, url))

    # Tamper with the size of the isolated local copy -> stale
    local_path <- BiocFileCache::bfcrpath(bfc, rids = bquery[["rid"]])
    writeLines("short", local_path)
    expect_true(BiocPkgTools:::.needs_update(bfc, bquery, url))
})

test_that("get_deprecated_status_df reports deprecated packages from a real VIEWS file", {
    skip_if_bioc_offline()

    version <- as.package_version(BiocManager:::.version_bioc("devel"))
    depdf <- BiocPkgTools:::get_deprecated_status_df(version)

    expect_s3_class(depdf, "data.frame")
    expect_identical(colnames(depdf), c("Package", "Deprecated", "PackageStatus"))
    expect_type(depdf[["Deprecated"]], "logical")
    expect_false(anyNA(depdf[["Deprecated"]]))
    expect_gt(nrow(depdf), 0L)
})

test_that("get_deprecated_status_df returns an empty frame when VIEWS has no rows", {
    testthat::local_mocked_bindings(
        .get_VIEWS = function(...) data.frame(),
        .package = "BiocPkgTools"
    )
    depdf <- BiocPkgTools:::get_deprecated_status_df("3.24")
    expect_identical(
        depdf,
        data.frame(
            Package = character(0L), Deprecated = logical(0L),
            PackageStatus = character(0L)
        )
    )
})

test_that(".import_dcf_stage_node parses a single summary dcf file", {
    tmp <- tempfile("dcf")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    node_dir <- file.path(tmp, "lconway")
    dir.create(node_dir)
    dcf_file <- file.path(node_dir, "buildsrc-summary.dcf")
    writeLines(c(
        "Package: foopkg",
        "StartedAt: 2024-01-01 00:00:00 -0000 (Mon, 01 Jan 2024)",
        "EndedAt: 2024-01-01 00:05:00 -0000 (Mon, 01 Jan 2024)",
        "EllapsedTime: 300.0 seconds"
    ), dcf_file)

    fields <- c("Package", "StartedAt", "EndedAt", "EllapsedTime")
    res <- BiocPkgTools:::.import_dcf_stage_node(dcf_file, fields = fields)

    expect_identical(
        names(res),
        c("Package", "node", "stage", "StartedAt", "EndedAt", "EllapsedTime")
    )
    expect_identical(unname(res["Package"]), "foopkg")
    expect_identical(unname(res["node"]), "lconway")
    expect_identical(unname(res["stage"]), "buildsrc")
})

test_that(".read_summary_dcfs combines summary dcfs across nodes/stages", {
    tmp <- tempfile("dcf")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    dir.create(file.path(tmp, "lconway"))
    dir.create(file.path(tmp, "nebbiolo2"))
    writeLines(c(
        "Package: foopkg",
        "StartedAt: 2024-01-01 00:00:00 -0000 (Mon, 01 Jan 2024)",
        "EndedAt: 2024-01-01 00:05:00 -0000 (Mon, 01 Jan 2024)",
        "EllapsedTime: 300.0 seconds"
    ), file.path(tmp, "lconway", "buildsrc-summary.dcf"))
    writeLines(c(
        "Package: barpkg",
        "StartedAt: 2024-01-02 00:00:00 -0000 (Tue, 02 Jan 2024)",
        "EndedAt: 2024-01-02 00:02:00 -0000 (Tue, 02 Jan 2024)",
        "EllapsedTime: 120.0 seconds"
    ), file.path(tmp, "nebbiolo2", "checksrc-summary.dcf"))

    res <- BiocPkgTools:::.read_summary_dcfs(tmp)

    expect_s3_class(res, "data.frame")
    expect_identical(nrow(res), 2L)
    expect_identical(
        colnames(res),
        c("Package", "node", "stage", "StartedAt", "EndedAt", "EllapsedTime")
    )
    expect_setequal(res[["Package"]], c("foopkg", "barpkg"))
    expect_setequal(res[["node"]], c("lconway", "nebbiolo2"))
    expect_setequal(res[["stage"]], c("buildsrc", "checksrc"))
})

test_that(".read_summary_dcfs returns an empty result when no summary dcfs exist", {
    tmp <- tempfile("dcf")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    res <- BiocPkgTools:::.read_summary_dcfs(tmp)
    expect_identical(nrow(res), 0L)
})
