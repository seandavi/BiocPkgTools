## generateBiocPkgDOI() calls the DataCite REST API to create (and, with
## `event = "publish"`, make findable) a DOI. Even in `testing = TRUE`
## ("sandbox") mode this mints a real, persistent-until-expiry DOI record
## against DataCite's test API -- that is a genuine external side effect,
## not something to trigger casually from an automated test run.
##
## The function reads credentials from the DATACITE_USERNAME/DATACITE_PASSWORD
## environment variables (this is true regardless of the `testing` flag --
## note the function does *not* default to the documented "apitest"/"apitest"
## sandbox login when no env vars are set, so without credentials it will
## simply fail DataCite's auth check). We only run the real test when both
## variables are populated (e.g. by a maintainer/CI secret); otherwise we
## skip with a clear explanation.
##
## The function has no separable "build the request" step to test the
## payload construction without performing the HTTP call, so there is no
## safe partial test available when credentials are absent.

test_that("generateBiocPkgDOI mints a sandbox DOI (requires DataCite credentials)", {
    skip_if_offline()
    skip_if_not(
        nzchar(Sys.getenv("DATACITE_USERNAME")) &&
            nzchar(Sys.getenv("DATACITE_PASSWORD")),
        paste(
            "DATACITE_USERNAME/DATACITE_PASSWORD are not set in this",
            "environment; skipping to avoid an unauthenticated DataCite API",
            "call (generateBiocPkgDOI() has no safe way to test its request",
            "construction without performing the HTTP call, and minting even",
            "a sandbox DOI is a real external side effect that requires",
            "credentials to opt into)"
        )
    )

    pkg <- paste0("TESTPKG", as.integer(Sys.time()))
    doi <- generateBiocPkgDOI(
        pkg, authors = "Test Author", pubyear = as.integer(format(Sys.Date(), "%Y")),
        event = "hide", testing = TRUE
    )
    expect_type(doi, "character")
    expect_true(grepl("10\\.82962/b9\\.bioc\\.", doi, ignore.case = TRUE))
})
