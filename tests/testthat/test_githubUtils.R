context("githubUtils")

# .gh_pkg_info() / githubDetails() call gh::gh() against the live GitHub
# API. Unauthenticated calls hit a very low (60 req/hr, shared across the
# whole machine) rate limit, so we skip gracefully rather than risk a flaky
# hard failure when no usable token is available. The `gh` package can pick
# up credentials from GITHUB_PAT/GITHUB_TOKEN *or* from a git credential
# helper (e.g. an authenticated `gh` CLI) -- gh::gh_whoami() is the
# reliable way to check whether *some* usable token is actually wired up,
# rather than just checking the env vars.
skip_if_no_github_token <- function() {
    testthat::skip_if_offline("github.com")
    who <- tryCatch(gh::gh_whoami(), error = function(e) NULL)
    if (is.null(who))
        testthat::skip(paste0(
            "No usable GitHub token available (checked GITHUB_PAT/GITHUB_TOKEN ",
            "and git credential helpers via gh::gh_whoami()); skipping to avoid ",
            "hitting unauthenticated GitHub API rate limits"
        ))
}

test_that(".gh_pkg_info returns repo details for a real repository", {
    skip_if_no_github_token()

    info <- BiocPkgTools:::.gh_pkg_info("seandavi/GEOquery")

    expect_s3_class(info, "gh_response")
    expect_identical(info[["full_name"]], "seandavi/GEOquery")
    expect_true(is.numeric(info[["stargazers_count"]]))
})

test_that(".gh_pkg_info warns and returns NA for a nonexistent repository", {
    skip_if_no_github_token()

    expect_warning(
        res <- BiocPkgTools:::.gh_pkg_info("seandavi/ThisRepoDoesNotExist12345XYZ"),
        "not found"
    )
    expect_true(is.na(res))
})

test_that("githubDetails fetches details for multiple real repos and drops failures", {
    skip_if_no_github_token()

    pkgs <- c("seandavi/GEOquery", "seandavi/ThisRepoDoesNotExist12345XYZ")
    ghd <- suppressWarnings(githubDetails(pkgs))

    expect_type(ghd, "list")
    # The nonexistent repo is dropped (NA results are filtered out)
    expect_identical(names(ghd), "seandavi/GEOquery")
    expect_identical(ghd[["seandavi/GEOquery"]][["full_name"]], "seandavi/GEOquery")
})

test_that("githubDetails respects the 'sleep' argument between calls", {
    skip_if_no_github_token()

    t <- system.time(
        invisible(githubDetails("seandavi/GEOquery", sleep = 0.5))
    )
    expect_gt(t[["elapsed"]], 0.4)
})
