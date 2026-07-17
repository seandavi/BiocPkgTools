context("biocPkgList")
library(BiocPkgTools)

bpkgl = biocPkgList()


test_that("pkg list has correct classes", {
    expect_is(bpkgl, 'tbl_df')
    expect_is(bpkgl, 'tbl')
    expect_is(bpkgl, 'data.frame')
})

test_that("ncol is approximately correct", {
  expect_gt(ncol(bpkgl), 35)
})

test_that("nrow is approximately correct", {
  expect_gt(nrow(bpkgl), 1000)
})

test_that("fnd column is present and is a list", {
  expect_true("fnd" %in% colnames(bpkgl))
  expect_type(bpkgl[["fnd"]], "list")
})

## --- CRAN-comparison helpers (biocPkgList() itself already tested above) ---

test_that("stripVersionString removes version constraints", {
  expect_identical(
    BiocPkgTools:::stripVersionString(
      c("pkgA (>= 1.2.3)", "pkgB", "pkgC (== 2.0)")
    ),
    c("pkgA", "pkgB", "pkgC")
  )
  ## no trailing parens -> unchanged
  expect_identical(BiocPkgTools:::stripVersionString("R"), "R")
  ## vector of length 0
  expect_identical(BiocPkgTools:::stripVersionString(character(0)), character(0))
})

test_that("CRAN_pkg_rds_url builds the expected URL", {
  url <- BiocPkgTools:::CRAN_pkg_rds_url()
  expect_type(url, "character")
  expect_length(url, 1L)
  expect_true(grepl("^https://cran\\.r-project\\.org", url))
  expect_true(grepl("packages\\.rds$", url))
})

test_that("read_CRAN_url reads a valid CRAN packages.rds", {
  ## NOTE: intentionally *not* testthat::skip_if_offline() here.
  ## skip_if_offline() calls skip_on_cran() internally, which skips
  ## whenever the NOT_CRAN env var isn't set to "true". The standard
  ## tests/testthat.R runner (testthat::test_check(), used by both
  ## `R CMD check` and covr's default test harness) never sets that
  ## variable, so skip_if_offline() would make these CRAN-network tests
  ## always-skipped under covr regardless of actual connectivity -- the
  ## exact same failure mode diagnosed for pkgBiocRevDeps() in
  ## test-pkgBiocDeps.R (see the comment there). A direct reachability
  ## check avoids that trap.
  testthat::skip_if_not(curl::has_internet(), "offline")
  url <- BiocPkgTools:::CRAN_pkg_rds_url()
  df <- BiocPkgTools:::read_CRAN_url(url)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 1000)
  expect_true("Package" %in% colnames(df))
})

test_that("read_CRAN_url errors on an unreadable URL", {
  ## NOTE: intentionally *not* testthat::skip_if_offline() here.
  ## skip_if_offline() calls skip_on_cran() internally, which skips
  ## whenever the NOT_CRAN env var isn't set to "true". The standard
  ## tests/testthat.R runner (testthat::test_check(), used by both
  ## `R CMD check` and covr's default test harness) never sets that
  ## variable, so skip_if_offline() would make these CRAN-network tests
  ## always-skipped under covr regardless of actual connectivity -- the
  ## exact same failure mode diagnosed for pkgBiocRevDeps() in
  ## test-pkgBiocDeps.R (see the comment there). A direct reachability
  ## check avoids that trap.
  testthat::skip_if_not(curl::has_internet(), "offline")
  ## the 404 also raises a base warning from readRDS()/url(), which is
  ## expected and not itself under test here
  suppressWarnings(expect_error(
    BiocPkgTools:::read_CRAN_url(
      "https://cran.r-project.org/web/packages/does-not-exist-xyz.rds"
    ),
    "'packages.rds' not found in CRAN 'repo'"
  ))
})

test_that("get_CRAN_pkg_rds returns full CRAN package data.frame", {
  ## NOTE: intentionally *not* testthat::skip_if_offline() here.
  ## skip_if_offline() calls skip_on_cran() internally, which skips
  ## whenever the NOT_CRAN env var isn't set to "true". The standard
  ## tests/testthat.R runner (testthat::test_check(), used by both
  ## `R CMD check` and covr's default test harness) never sets that
  ## variable, so skip_if_offline() would make these CRAN-network tests
  ## always-skipped under covr regardless of actual connectivity -- the
  ## exact same failure mode diagnosed for pkgBiocRevDeps() in
  ## test-pkgBiocDeps.R (see the comment there). A direct reachability
  ## check avoids that trap.
  testthat::skip_if_not(curl::has_internet(), "offline")
  df <- BiocPkgTools:::get_CRAN_pkg_rds()
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 1000)
  expect_true(all(c("Package", "Version") %in% colnames(df)))
})

