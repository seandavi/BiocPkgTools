## Network gating for the test suite.
##
## Set BIOCPKGTOOLS_NO_NETWORK=true to skip every test that needs the
## network, e.g. for a fast offline run or a sandbox with no egress:
##
##   BIOCPKGTOOLS_NO_NETWORK=true Rscript -e 'devtools::test()'

#' Is network access disabled for the test suite?
network_disabled <- function() {
    isTRUE(as.logical(Sys.getenv("BIOCPKGTOOLS_NO_NETWORK", "false")))
}

.bioc_reachable_cache <- new.env(parent = emptyenv())

.bioc_reachable <- function(timeout_sec = 5) {
    if (network_disabled() || !curl::has_internet())
        return(FALSE)
    ## probe once per test run, not once per test
    if (!is.null(.bioc_reachable_cache$ok))
        return(.bioc_reachable_cache$ok)
    ok <- tryCatch({
        req <- httr2::request("https://bioconductor.org")
        req <- httr2::req_options(req, timeout_ms = timeout_sec * 1000)
        req <- httr2::req_method(req, "HEAD")
        resp <- httr2::req_perform(req)
        httr2::resp_status(resp) < 400
    }, error = function(e) FALSE)
    .bioc_reachable_cache$ok <- isTRUE(ok)
    .bioc_reachable_cache$ok
}

#' Skip a test unless bioconductor.org is reachable
#'
#' Use in place of `testthat::skip_if_offline()` for tests that hit
#' Bioconductor-hosted resources specifically (general internet access does
#' not guarantee bioconductor.org is up).
skip_if_bioc_offline <- function() {
    testthat::skip_on_cran()
    testthat::skip_if_not(
        .bioc_reachable(), "bioconductor.org is not reachable"
    )
}

#' Skip a test unless general internet access is available
#'
#' Unlike `testthat::skip_if_offline()`, this does not also skip on CRAN.
skip_if_no_internet <- function() {
    testthat::skip_if(network_disabled(), "BIOCPKGTOOLS_NO_NETWORK is set")
    testthat::skip_if_not(curl::has_internet(), "offline")
}

#' `testthat::skip_if_offline()` that also honors BIOCPKGTOOLS_NO_NETWORK
skip_if_offline <- function(host = "captive.apple.com") {
    testthat::skip_if(network_disabled(), "BIOCPKGTOOLS_NO_NETWORK is set")
    testthat::skip_if_offline(host)
}
