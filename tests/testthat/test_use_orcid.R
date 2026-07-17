## use_orcid.R hits the public ORCID API. Unlike DataCite, ".get_orcid_token()"
## uses a client id/secret that are already embedded in the package source
## for a public, read-only ("/read-public") OAuth client-credentials flow --
## no user-supplied token or credentials are required, so the ORCID-facing
## tests below run as real, live requests whenever the network is available
## (no skip-for-credentials needed). ".get_cre_orcid()"/"get_cre_orcids()"
## are purely local (they read installed packages' DESCRIPTION metadata) and
## need no network at all.

test_that(".get_orcid_token obtains a live bearer token from ORCID", {
    skip_if_offline()

    token <- BiocPkgTools:::.get_orcid_token()
    expect_s3_class(token, "httr2_token")
    expect_identical(token[["token_type"]], "bearer")
    expect_true(nzchar(token[["access_token"]]))
})

test_that(".get_orcid_endpoint and .bind_employments fetch real employment data", {
    skip_if_offline()

    token <- BiocPkgTools:::.get_orcid_token()
    orcid_id <- "0000-0002-3242-0582" ## Marcel Ramos, a package author here

    raw <- BiocPkgTools:::.get_orcid_endpoint(orcid_id, "employments", token)
    expect_true("affiliation-group" %in% names(raw))

    employments <- BiocPkgTools:::.bind_employments(orcid_id, token)
    expect_s3_class(employments, "data.frame")
    expect_gt(nrow(employments), 0L)
    expect_true(
        "employment-summary.organization.name" %in% colnames(employments)
    )
})

test_that("orcid_table returns employment data for one or more ORCID ids", {
    skip_if_offline()

    single <- orcid_table(orcids = "0000-0002-3242-0582")
    expect_s3_class(single, "data.frame")
    expect_gt(nrow(single), 0L)

    multi <- orcid_table(
        orcids = c("0000-0002-3242-0582", "0000-0002-8991-6458")
    )
    expect_gte(nrow(multi), nrow(single))
    expect_true(
        length(unique(multi[["employment-summary.source.source-orcid.path"]])) >= 2
    )
})

test_that("orcid_table errors for an ORCID id that does not exist", {
    skip_if_offline()

    expect_error(orcid_table(orcids = "0000-0000-0000-0000"))
})

test_that(".get_cre_orcid extracts the ORCID for the 'cre' author, if present", {
    ## BiocPkgTools' own DESCRIPTION lists Sean Davis as 'cre' with an ORCID
    orcid <- BiocPkgTools:::.get_cre_orcid("BiocPkgTools")
    expect_identical(orcid, "0000-0002-8991-6458")

    ## edge case: a package with no Authors@R / no ORCID for its 'cre' should
    ## return NA rather than erroring
    expect_identical(BiocPkgTools:::.get_cre_orcid("utils"), NA_character_)
})

test_that("get_cre_orcids vectorizes over multiple installed packages", {
    result <- get_cre_orcids(c("BiocPkgTools", "utils"))
    expect_type(result, "character")
    expect_identical(names(result), c("BiocPkgTools", "utils"))
    expect_identical(unname(result["BiocPkgTools"]), "0000-0002-8991-6458")
    expect_true(is.na(result["utils"]))
})
