utils::globalVariables(
    c(".data", "biocViewsVocab", "Nb_of_distinct_IPs", "Nb_of_downloads")
)

.cacheRead <- function(uri) {
    ufile <- .cache_url_file(uri)
    read.table(ufile, header = TRUE)
}

repo_short_names <- data.frame(
  repository =
    c("software", "data-experiment", "workflows", "data-annotation", "books"),
  stat.url =
    c("bioc", "data-experiment", "workflows", "data-annotation", NA_character_),
  stat.file =
    c("bioc", "experiment", "workflows", "annotation", NA_character_),
  json.file =
    c("bioc", "data/experiment", "workflows", "data/annotation", NA_character_),
  repo.name =
    c("BioCsoft", "BioCexp", "BioCworkflows", "BioCann", "BioCbooks")
)

#' Get Bioconductor download statistics
#'
#' @details Note that Bioconductor package download
#' stats are not version-specific.
#'
#' @param pkgType character(1) Either one of 'software', 'data-experiment',
#'     'workflows', or 'data-annotation' (defaults to 'all' or 'software')
#'
#' @importFrom dplyr mutate
#' @importFrom utils read.table
#' @importFrom tibble as_tibble
#' @importFrom BiocFileCache BiocFileCache bfcupdate bfcneedsupdate bfcrpath
#'     bfcquery bfcnew
#'
#' @return A \code{tibble} of download statistics for all Bioconductor packages
#'
#' @examples
#' biocDownloadStats()
#'
#' @export
biocDownloadStats <-
    function(
        pkgType = c(
            "all", "software", "data-experiment",
            "workflows", "data-annotation"
        )
    )
{

    formal.args <- eval(formals()[["pkgType"]])
    pkgType <- match.arg(pkgType)
    pkgType <- switch(pkgType, all = tail(formal.args, -1), pkgType)
    linkPkg <- .matchGetShortName(pkgType, "stat.url")
    fnameType <- .matchGetShortName(pkgType, "stat.file")

    base_url <- "http://bioconductor.org/packages/stats/%s/%s_pkg_stats.tab"

    stats_urls <- sprintf(base_url,
        linkPkg, fnameType
    )

    tlist <- lapply(stats_urls, .cacheRead)
    tbl <- as_tibble(
        cbind(
            pkgType = rep(pkgType, vapply(tlist, nrow, numeric(1L))),
            dplyr::bind_rows(tlist)
        )
    )

    tbl <- filter(tbl, .data$Month != "all") |>
        dplyr::mutate(
            Date = as.Date(
                paste(.data$Year, .data$Month, '01'), '%Y %b %d'
            )
        )

    class(tbl) <- c('bioc_downloads', class(tbl))
    tbl
}

.try.read.table <- function(...) {
    tryCatch({
        read.table(...)
    }, error = function(err) {
        data.frame(
            Year = integer(0L), Month = character(0L),
            Nb_of_distinct_IPs = integer(0L), Nb_of_downloads = integer(0L)
        )
    })
}

#' Get Bioconductor download statistics for a package
#'
#' @param pkg character(1) The name of a Bioconductor package
#'
#' @param years numeric(), character() A vector of years from which to
#'     obtain download statistics (defaults to current year)
#'
#' @inheritParams biocDownloadStats
#'
#' @return A \code{tibble} of download statistics
#'
#' @examples
#'
#' pkgDownloadStats("GenomicRanges")
#'
#' @export
pkgDownloadStats <-
    function(
        pkg,
        pkgType = c(
            "software", "data-experiment", "workflows", "data-annotation"
        ),
        years = format(Sys.time(), "%Y")
    )
{

    pkgType <- match.arg(pkgType)
    pkgType <- .matchGetShortName(pkgType, "stat.url")

    base_url <- "http://bioconductor.org/packages/stats/%s/%s/%s_%s_stats.tab"

    stats_urls <- sprintf(base_url,
        pkgType, pkg, pkg, years
    )

    tlist <- lapply(stats_urls, .try.read.table, header = TRUE)
    tbl <- as_tibble(dplyr::bind_rows(tlist))
    filter(tbl,
        .data$Month != "all" &
            (Nb_of_distinct_IPs != 0 & Nb_of_downloads != 0)
    )

}

#' When did a package enter Bioconductor?
#'
#' This function uses the biocDownloadStats
#' data to *approximate* when a package entered
#' Bioconductor. Note that the download stats
#' go back only to 2009.
#'
#' @importFrom dplyr filter group_by top_n collect
#'
#' @param download_stats a data.frame from \code{\link{biocDownloadStats}}
#'
#' @examples
#'
#' dls <- biocDownloadStats()
#' tail(firstInBioc(dls))
#'
#' @export
firstInBioc <- function(download_stats) {
  download_stats |>
    dplyr::filter(.data$Month!='all') |>
    dplyr::group_by(.data$Package) |>
    # thanks: https://stackoverflow.com/questions/43832434/arrange-within-a-group-with-dplyr
    dplyr::top_n(1, dplyr::desc(.data$Date)) |>
    dplyr::collect()
}

.matchGetShortName <- function(pkgType, colName) {
    repo_short_names[match(pkgType, repo_short_names[["repository"]]), colName]
}

