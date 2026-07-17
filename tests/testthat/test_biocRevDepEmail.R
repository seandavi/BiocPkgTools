## biocRevDepEmail() can send a real email via 'blastula' when
## `dry.run = FALSE` and `textOnly = FALSE`. We never do that here. Instead,
## we exercise the safe `textOnly = TRUE, dry.run = TRUE` path, which builds
## the message text locally and never calls out to an SMTP server -- this is
## also exactly the pattern used in the function's own `@examples`. The
## user-info cache is redirected to a temp dir so the test never touches (or
## depends on) a developer's real BiocFileCache-based credentials cache.

test_that(".biocPkgsLinks builds Bioconductor build-report markdown/URL links", {
    expect_identical(
        BiocPkgTools:::.biocPkgsLinks(character(0), version = "3.21"),
        NULL
    )
    md <- BiocPkgTools:::.biocPkgsLinks(
        c("S4Vectors", "IRanges"), version = "3.21"
    )
    expect_identical(
        md,
        c(
            "[S4Vectors](https://bioconductor.org/checkResults/3.21/bioc-LATEST/S4Vectors)",
            "[IRanges](https://bioconductor.org/checkResults/3.21/bioc-LATEST/IRanges)"
        )
    )
    plain <- BiocPkgTools:::.biocPkgsLinks(
        "S4Vectors", version = "3.21", md = FALSE
    )
    expect_identical(
        plain, "https://bioconductor.org/checkResults/3.21/bioc-LATEST/S4Vectors"
    )
})

test_that(".CRANpkgsLinks builds CRAN package page markdown/URL links", {
    expect_identical(
        BiocPkgTools:::.CRANpkgsLinks(character(0)), NULL
    )
    md <- BiocPkgTools:::.CRANpkgsLinks(c("jsonlite"))
    expect_identical(
        md, "[jsonlite](https://cran.r-project.org/package=jsonlite)"
    )
    plain <- BiocPkgTools:::.CRANpkgsLinks("jsonlite", md = FALSE)
    expect_identical(plain, "https://cran.r-project.org/package=jsonlite")
})

test_that(".pkgsLinks dispatches Bioconductor vs CRAN packages to the right helper", {
    skip_if_bioc_offline()
    skip_if_offline()

    res <- BiocPkgTools:::.pkgsLinks(
        c("S4Vectors", "jsonlite"), version = "3.21"
    )
    expect_true(any(grepl("bioconductor.org", res)))
    expect_true(any(grepl("cran.r-project.org", res)))
})

test_that("biocRevDepEmail builds a dry-run, text-only deprecation notice", {
    skip_if_bioc_offline()

    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    ## Same call as the function's own @examples: a historical, static
    ## Bioconductor release means the set of reverse dependencies is fixed
    ## and the test result is stable over time.
    send <- biocRevDepEmail(
        "FindMyFriends", version = "3.13", dry.run = TRUE, textOnly = TRUE,
        core.name = "Test User", core.email = "test@example.com",
        core.id = "TU00001"
    )

    expect_type(send, "character")
    expect_true(any(grepl("Package(s) Deprecation Notification", send, fixed = TRUE)))
    expect_true(any(grepl("PanVizGenerator", send)))
})

test_that("biocRevDepEmail errors when there are no reverse dependencies", {
    skip_if_bioc_offline()

    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    expect_error(
        biocRevDepEmail(
            "NoSuchBiocPackageXYZ123", version = BiocManager::version(),
            dry.run = TRUE, textOnly = TRUE,
            core.name = "Test User", core.email = "test@example.com",
            core.id = "TU00001"
        ),
        "No reverse dependencies on"
    )
})

test_that("biocRevDepEmail requires an existing email template file", {
    expect_error(
        biocRevDepEmail(
            "SomePkg", emailTemplate = tempfile(fileext = ".Rmd"),
            core.name = "Test User", core.email = "test@example.com",
            core.id = "TU00001"
        ),
        "'emailTemplate' file not found"
    )
})
