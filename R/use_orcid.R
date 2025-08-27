.COLS_OF_INTEREST <- c(
    "employment-summary.source.source-name.value",
    "employment-summary.organization.name",
    "employment-summary.source.source-orcid.path",
    "employment-summary.source.source-orcid.uri",
    "employment-summary.put-code",
    "employment-summary.path",
    "employment-summary.organization.address.city",
    "employment-summary.organization.address.country"
)

.get_orcid_token <- function(req) {
    orcid_app <-  oauth_client(
        id = "APP-OSPJCJPZ6P4I3DF0",
        secret = obfuscated(
            paste0(
                "zx3gSdnxRyInqsP0f_72jT0JO54IMQA-",
                "Vgx2hblcIEGEcDDt2VpWaLgeHO8LHRVffOUY4w"
            )
        ),
        token_url = "https://orcid.org/oauth/token",
        name = "biocreporting_orcid"
    )
    oauth_flow_client_credentials(
        client = orcid_app,
        scope = "/read-public"
    )
}

.get_orcid_endpoint <- function(orcid_id, endpoint, token) {
    request("https://pub.orcid.org/") |>
        req_template(
            "v3.0/{orcid}/{endpoint}",
            orcid = orcid_id,
            endpoint = endpoint
        ) |>
        req_headers(
            Accept = "application/vnd.orcid+json"
        ) |>
        req_auth_bearer_token(token$access_token) |>
        req_perform() |>
        resp_body_json()
}

.bind_employments <- function(orcid_id, token) {
    result <- .get_orcid_endpoint(orcid_id, "employments", token)
    affiliation_groups <- lapply(
        result[["affiliation-group"]],
        function(affiliation_group) {
            resframe <- unlist(affiliation_group[["summaries"]][[1L]]) |>
                t() |>
                as.data.frame()
            resframe[, .COLS_OF_INTEREST]
        }
    )
    do.call(rbind.data.frame, affiliation_groups)
}

#' @title Obtain employment data from ORCID
#'
#' @description Get a `data.frame` of employment info from ORCID
#'
#' @importFrom httr2 request req_headers req_body_form req_method req_perform
#'   resp_body_json req_template req_auth_bearer_token
#'   oauth_flow_client_credentials oauth_client obfuscated
#'
#' @param orcids `character()` A vector of ORCID identifiers
#'
#' @returns a `data.frame` of employment info using the ORCID API
#'
#' @examples
#' if (interactive()) {
#'     orcid_table(
#'         orcids = c(
#'             "0000-0002-3242-0582",
#'             "0000-0003-4046-0063",
#'             "0000-0003-2725-0694"
#'         )
#'     )
#' }
#' @export
orcid_table <- function(orcids) {
    token <- .get_orcid_token()
    results <- lapply(
        orcids,
        function(orcid, token) {
            .bind_employments(orcid, token)
        }, token = token
    )
    do.call(rbind.data.frame, results)
}

#' get the ORCID id from cre field of Authors@R in packageDescription result
#' @param pkgname character(1)
.get_cre_orcid <- function(pkgname) {
    stopifnot(length(pkgname) == 1)
    de <- packageDescription(pkgname)
    nde <- names(de)
    if (!("Authors@R" %in% nde)) {
        return(NA_character_)
    }
    au <- de[["Authors@R"]]
    vec <- eval(parse(text = au))
    cre <- grep("\\[.*cre.*\\]", vec, value = TRUE)
    hasorc <- grep("orcid.org", cre)
    if (length(hasorc) == 0) {
        return(NA_character_)
    }
    gsub(".*orcid.org/(.*)>.*", "\\1", cre)
}

#' get ORCID ids from cre fields of Authors@R in packageDescription results
#' @param pkgnames `character()` must be installed
#' @note returns NA if no ORCID provided in Authors@R for package description
#' @examples
#' get_cre_orcids(c("BiocPkgTools", "utils"))
#' @export
get_cre_orcids <- function(pkgnames) {
    vapply(pkgnames, .get_cre_orcid, character(1))
}
