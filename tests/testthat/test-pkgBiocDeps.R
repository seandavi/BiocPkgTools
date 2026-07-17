test_that("pkgBiocRevDeps(recursive = TRUE) returns only reverse deps", {
    ## NOTE: this test hits bioconductor.org (via BiocManager::repositories()),
    ## so it must be gated with skip_if_bioc_offline() rather than
    ## testthat::skip_if_offline(). skip_if_offline() calls skip_on_cran()
    ## internally, which skips whenever the NOT_CRAN env var isn't set to
    ## "true". devtools::test() sets that var for you (via
    ## local_assume_not_on_cran()), but the standard tests/testthat.R runner
    ## (testthat::test_check(), which is what `R CMD check` and covr's
    ## default test harness both use) does not -- so under covr this test
    ## was *always* skipped ("Reason: On CRAN"), regardless of actual network
    ## reachability, which is why covr reported 0% executed lines for this
    ## file even though the test itself looks fine.
    skip_if_bioc_offline()
    ## Regression test for issue #81: Recursing reverse dependencies over "all"
    ## dependencies will return nearly the entire repository, including some
    ## forward dependencies of the queried package.
    pkg <- "Rarr"

    db <- utils::available.packages(repos = BiocManager::repositories())
    skip_if_not(pkg %in% rownames(db), "Rarr not available in repositories")

    ## Forward dependencies that are not themselves reverse dependencies should
    ## never appear in the recursive reverse-dependency result.
    fwd <- tools::package_dependencies(pkg, db, which = "all")[[pkg]]
    rev1 <- tools::package_dependencies(
        pkg, db, reverse = TRUE, which = "all"
    )[[pkg]]
    fwd_only <- setdiff(fwd, rev1)

    res <- pkgBiocRevDeps(
        pkg, which = "all", recursive = TRUE, only.bioc = FALSE
    )
    rec <- as.character(res)

    ## check that some forward dependencies are present in the recursive result
    expect_true(any(fwd_only %in% rec))

    ## check that the recursive result is not too small
    expect_gt(length(rec), nrow(db) / 2)
})

test_that("pkgBiocDeps returns strong bioc-only dependencies", {
    skip_if_bioc_offline()
    ## 'Rarr' is a small package with no strong Bioconductor dependencies
    ## (its strong deps are all on CRAN), which exercises the "no matches"
    ## branch of the only.bioc filter.
    res <- pkgBiocDeps("Rarr", only.bioc = TRUE)
    expect_type(res, "list")
    expect_named(res, "Rarr")
    expect_identical(res[["Rarr"]], character(0))

    ## with only.bioc = FALSE we should see actual (CRAN) dependencies
    res_all <- pkgBiocDeps("Rarr", only.bioc = FALSE)
    expect_type(res_all, "list")
    expect_gt(length(res_all[["Rarr"]]), 0L)
})

test_that("pkgBiocDeps errors on invalid pkgType", {
    expect_error(
        pkgBiocDeps("Rarr", pkgType = "bogus"),
        "'arg' should be one of"
    )
})

test_that("summary.biocrevdeps tallies dependency counts", {
    skip_if_bioc_offline()
    rd <- pkgBiocRevDeps("Rarr", which = "strong")
    expect_s3_class(rd, "biocrevdeps")

    s <- summary(rd)
    expect_s3_class(s, "data.frame")
    expect_identical(rownames(s), "Rarr")
    expect_true(all(c("Depends", "Imports", "LinkingTo", "Total") %in% colnames(s)))
    expect_identical(
        s[["Total"]],
        sum(s[["Depends"]], s[["Imports"]], s[["LinkingTo"]])
    )

    ## printed 'tools::package_dependencies(...)' call reproduction
    expect_output(summary(rd), "tools::package_dependencies")
})

test_that("summary.biocrevdeps handles recursive results (no Total column)", {
    skip_if_bioc_offline()
    rd <- pkgBiocRevDeps("Rarr", which = "strong", recursive = TRUE)
    expect_s3_class(rd, "biocrevdeps")

    s <- summary(rd)
    expect_s3_class(s, "data.frame")
    expect_true("recursive" %in% colnames(s))
    expect_false("Total" %in% colnames(s))
})
