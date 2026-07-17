## CRANstatus() unconditionally requires the 'blastula' and 'kableExtra'
## packages (both Suggests, neither of which is a hard testing dependency),
## and its only-real path culminates in constructing (and, if
## `dry.run = FALSE`, sending) an email. We test:
##  1. the argument-validation / dependency guard, which is real production
##     logic and safe to exercise regardless of whether the optional
##     packages happen to be installed.
##  2. the live CRAN scrape + status parsing, when 'blastula' and
##     'kableExtra' are both available -- this never sends an email because
##     `dry.run = TRUE` is the default and we never pass `dry.run = FALSE`.

test_that("CRANstatus validates 'pkg' argument", {
    expect_error(CRANstatus(character(0)))
    expect_error(CRANstatus(c("a", "b")))
    expect_error(CRANstatus(NA_character_))
})

test_that("CRANstatus requires 'blastula' and 'kableExtra' to be installed", {
    have_deps <- requireNamespace("blastula", quietly = TRUE) &&
        requireNamespace("kableExtra", quietly = TRUE)
    skip_if(
        have_deps,
        "'blastula' and 'kableExtra' are both installed; guard clause is not triggered"
    )

    expect_error(
        CRANstatus("jsonlite"),
        "Install the '(blastula|kableExtra)' package"
    )
})

test_that("CRANstatus reports OK status for a healthy CRAN package (dry run)", {
    skip_if_offline()
    skip_if_not_installed("blastula")
    skip_if_not_installed("kableExtra")

    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    ## jsonlite is a long-standing, actively-maintained CRAN package that is
    ## very unlikely to have an ERROR on its check_results page; this keeps
    ## the test from attempting to render/send an email via blastula.
    result <- CRANstatus(
        "jsonlite", dry.run = TRUE,
        core.name = "Test User", core.email = "test@example.com",
        core.id = "TU00001"
    )
    expect_named(result, "OK")
    expect_type(result, "logical")
})
