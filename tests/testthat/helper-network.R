.bioc_reachable <- function(timeout_sec = 5) {
    if (!curl::has_internet())
        return(FALSE)
    ok <- tryCatch({
        req <- httr2::request("https://bioconductor.org")
        req <- httr2::req_options(req, timeout_ms = timeout_sec * 1000)
        req <- httr2::req_method(req, "HEAD")
        resp <- httr2::req_perform(req)
        httr2::resp_status(resp) < 400
    }, error = function(e) FALSE)
    isTRUE(ok)
}

#' Skip a test unless bioconductor.org is reachable
#'
#' Use in place of `testthat::skip_if_offline()` for tests that hit
#' Bioconductor-hosted resources specifically (general internet access does
#' not guarantee bioconductor.org is up).
skip_if_bioc_offline <- function() {
    testthat::skip_if_not(
        .bioc_reachable(), "bioconductor.org is not reachable"
    )
}
