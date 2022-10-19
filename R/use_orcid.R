#' process employment data from ORCID
#' @import rorcid
#' @param orcid character(1)
#' @param rename logical(1) if TRUE use short names
.get_orcid_rec <- function(orcid, rename = TRUE) {
    #
    # fields returned by API using rorcid package, Oct 2022
    #
    kp <- c(
        "employment-summary.source.source-name.value",
        "employment-summary.organization.name",
        "employment-summary.organization.address.city",
        "employment-summary.organization.address.region",
        "employment-summary.organization.address.country"
    )
    #
    # fault tolerance -- in case affiliation-group data are not populated
    # return a slug of NAs
    #
    nfields <- length(kp)
    sna <- rep(NA, nfields)
    # needs to reflect order in kp above
    sslug <- c("name", "org", "city", "region", "country")
    if (rename) names(sna) <- sslug else names(sna) <- kp
    #
    # try the API via rorcid
    #
    if (!is.na(orcid)) {
        ee <- orcid_employments(orcid)
        dat <- try(
            t(ee[[orcid]][["affiliation-group"]][["summaries"]][[1]]),
            silent = TRUE
        ) # should be more precise than try()
    }
    if (is.na(orcid) || inherits(dat, "try-error")) {
        ans <- matrix(sna, nrow = 1)
        colnames(ans) <- names(sna)
        ans <- data.frame(ans)
        ans$orcid <- orcid
        return(ans)
    }
    #
    # made it
    #
    ans <- dat[kp, ]
    if (rename) {
        names(ans) <- c("name", "org", "city", "region", "country")
    }
    ans <- data.frame(t(ans))
    ans$orcid <- orcid
    rownames(ans) <- NULL
    ans
}

#' get data.frame of employment info from orcid
#' @param orcids character()
#' @examples
#' if (interactive()) { # need a token?
#'     oids <- c("0000-0003-4046-0063", "0000-0003-4046-0063")
#'     print(orcid_table(oids))
#'     oids <- c(oids, NA)
#'     print(orcid_table(oids))
#'     print(orcid_table(oids[1]))
#' }
#' @export
orcid_table <- function(orcids) {
    ans <- lapply(orcids, .get_orcid_rec)
    do.call(rbind, ans)
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
#' @param pkgnames character() must be installed
#' @note returns NA if no ORCID provided in Authors@R for package description
#' @examples
#' get_cre_orcids(c("BiocPkgTools", "utils"))
#' @export
get_cre_orcids <- function(pkgnames) {
    vapply(pkgnames, .get_cre_orcid, character(1))
}
