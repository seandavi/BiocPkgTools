context("problemPage")

# ---------------------------------------------------------------------------
# chkURL() is pure string construction: no network needed at all.
# ---------------------------------------------------------------------------

test_that("chkURL builds the standard per-stage URL for non-skipped results", {
    url <- chkURL(
        ver = "devel", result = "ERROR", pack = "BiocOncoTK",
        node = "malbec1", stage = "buildsrc"
    )
    expect_identical(
        url,
        "https://bioconductor.org/checkResults/devel/bioc-LATEST/BiocOncoTK/malbec1-buildsrc.html"
    )
})

test_that("chkURL falls back to the package landing page for skipped/NA results", {
    url_skipped <- chkURL(
        ver = "devel", result = "skipped", pack = "BiocOncoTK",
        node = "malbec1", stage = "buildsrc"
    )
    expect_identical(
        url_skipped,
        "https://bioconductor.org/checkResults/devel/bioc-LATEST/BiocOncoTK/"
    )

    url_na <- chkURL(
        ver = "devel", result = "NA", pack = "BiocOncoTK",
        node = "malbec1", stage = "buildsrc"
    )
    expect_identical(
        url_na,
        "https://bioconductor.org/checkResults/devel/bioc-LATEST/BiocOncoTK/"
    )
})

test_that("chkURL is vectorized and mixes both URL forms", {
    urls <- chkURL(
        ver = c("devel", "devel"),
        result = c("ERROR", "skipped"),
        pack = c("pkgA", "pkgB"),
        node = c("nodeA", "nodeB"),
        stage = c("checksrc", "buildsrc")
    )
    expect_length(urls, 2L)
    expect_identical(
        urls[1],
        "https://bioconductor.org/checkResults/devel/bioc-LATEST/pkgA/nodeA-checksrc.html"
    )
    expect_identical(
        urls[2],
        "https://bioconductor.org/checkResults/devel/bioc-LATEST/pkgB/"
    )
})

# ---------------------------------------------------------------------------
# checkMe() / problemPage() -- real Bioconductor build-report data.
#
# checkMe() and checkDeps() both call biocBuildReport(ver) internally with
# its default 'pkgType' (all four package types), so there is no way to
# narrow the fetch itself -- narrowing only affects post-filtering. To avoid
# paying for that expensive fetch twice, this single test does ONE real
# fetch via checkMe() directly, then reuses that same real result to drive
# problemPage() by stubbing checkMe() (its only internal dependency for the
# default authorPattern code path) to return the already-fetched data. This
# still validates problemPage()'s own link-building/DT logic against
# genuine live Bioconductor data, it just avoids re-triggering the network
# fetch a second time for what would be the same call.
# ---------------------------------------------------------------------------

test_that("checkMe and problemPage surface real problems, including a non-software package type (#21)", {
    skip_if_bioc_offline()

    mm <- checkMe(ver = "devel", authorPattern = "V.*Carey")

    expect_s3_class(mm, "data.frame")
    expect_gt(nrow(mm), 0L)
    expect_true(all(
        c("pkg", "author", "pkgType", "node", "stage", "result", "bioc_version") %in%
            colnames(mm)
    ))
    # includeOK defaults to FALSE: only non-OK rows should be returned
    expect_false("OK" %in% mm[["result"]])
    expect_true(all(grepl("Carey", mm[["author"]], ignore.case = TRUE)))

    # Regression check for #21 ("could problemPage also check workflow
    # packages?"): biocBuildReport() used to inner-join away non-software
    # package types, so checkMe()/checkDeps() would silently never surface
    # them even though they can have real build problems. Confirm at least
    # one non-software package type is present in the real result.
    non_soft <- mm[mm[["pkgType"]] != "bioc", ]
    expect_gt(nrow(non_soft), 0L)

    # Reuse the already-fetched real data to exercise problemPage()'s own
    # link-building/DT::datatable logic without a second live fetch.
    testthat::local_mocked_bindings(checkMe = function(...) mm)

    pp <- problemPage(authorPattern = "V.*Carey", ver = "devel")
    expect_s3_class(pp, "datatables")
    expect_s3_class(pp, "htmlwidget")

    dt_data <- pp[["x"]][["data"]]
    expect_equal(nrow(dt_data), nrow(mm))

    # Every package cell should be an anchor tag pointing at the URL
    # produced by chkURL() for that row.
    expect_true(all(grepl("^<a href=", dt_data[["package"]])))

    non_soft_idx <- which(mm[["pkgType"]] != "bioc")
    expect_true(all(
        grepl(mm[["pkg"]][non_soft_idx], dt_data[["package"]][non_soft_idx])
    ))
})

test_that("checkMe errors informatively when authorPattern matches nothing", {
    # Stub the (expensive) internal biocBuildReport() call with a tiny,
    # clearly-fake data frame: we're only testing checkMe()'s own zero-match
    # validation logic here, not Bioconductor data, so there's no need to
    # pay for a live fetch.
    fake_report <- data.frame(
        pkg = c("pkgA", "pkgB"),
        author = c("Alice Smith", "Bob Jones"),
        pkgType = c("bioc", "bioc"),
        result = c("OK", "WARNINGS"),
        stringsAsFactors = FALSE
    )
    testthat::local_mocked_bindings(
        biocBuildReport = function(...) fake_report
    )

    expect_error(
        checkMe(authorPattern = "ZZZ-No-Such-Author-Pattern-999"),
        "zero results",
        fixed = FALSE
    )
})

# ---------------------------------------------------------------------------
# checkDeps() -- main path against real dependency + build-report data.
# ---------------------------------------------------------------------------

test_that("checkDeps requires a 'dependsOn' argument", {
    expect_error(
        checkDeps(),
        "must provide a package name"
    )
})

test_that("checkDeps finds real dependent packages and their build status", {
    skip_if_bioc_offline()

    mine <- checkDeps(dependsOn = "limma", ver = "devel")

    expect_s3_class(mine, "data.frame")
    expect_gt(nrow(mine), 0L)
    expect_true(all(
        c("pkg", "author", "pkgType", "node", "stage", "result", "bioc_version") %in%
            colnames(mine)
    ))
    expect_false("OK" %in% mine[["result"]])

    # Sanity check: the returned packages really are direct dependents of
    # limma, cross-referenced against the real dependency graph.
    all_pkg_deps <- buildPkgDependencyDataFrame()
    limma_dependents <- all_pkg_deps[
        all_pkg_deps[["dependency"]] == "limma", "Package", drop = TRUE
    ]
    expect_true(all(mine[["pkg"]] %in% limma_dependents))
})

test_that("checkDeps errors informatively when no dependent packages are found", {
    # As with the checkMe() zero-match test above, this is validating
    # checkDeps()'s own "nothing found" logic, not Bioconductor data, so we
    # stub its two (expensive) internal data fetches with tiny fakes that
    # are guaranteed not to match, rather than paying for two live fetches.
    fake_report <- data.frame(
        pkg = c("pkgA", "pkgB"),
        author = c("Alice Smith", "Bob Jones"),
        pkgType = c("bioc", "bioc"),
        result = c("OK", "WARNINGS"),
        stringsAsFactors = FALSE
    )
    fake_deps <- data.frame(
        Package = character(0),
        dependency = character(0),
        edgetype = character(0),
        stringsAsFactors = FALSE
    )
    testthat::local_mocked_bindings(
        biocBuildReport = function(...) fake_report,
        buildPkgDependencyDataFrame = function(...) fake_deps
    )

    expect_error(
        checkDeps(dependsOn = "ZZZ-No-Such-Package-999"),
        "No dependent packages found"
    )
})

# ---------------------------------------------------------------------------
# problemPage() argument validation (no network required).
# ---------------------------------------------------------------------------

test_that("problemPage warns and falls back to authorPattern when both args given", {
    fake_mm <- data.frame(
        pkg = "pkgA", author = "Alice", result = "WARNINGS",
        node = "nodeA", stage = "buildsrc", bioc_version = "3.24",
        stringsAsFactors = FALSE
    )
    testthat::local_mocked_bindings(checkMe = function(...) fake_mm)

    expect_warning(
        problemPage(authorPattern = "Alice", dependsOn = "limma"),
        "Both.*authorPattern.*dependsOn"
    )
})

test_that("problemPage errors when there is nothing to report", {
    testthat::local_mocked_bindings(checkMe = function(...) NULL)
    expect_error(problemPage(authorPattern = "Alice"), "all packages fine")
})
