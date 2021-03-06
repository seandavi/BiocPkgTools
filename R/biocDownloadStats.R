utils::globalVariables(c(".data", "biocViewsVocab"))

#' Get Bioconductor download statistics
#'
#' @details Note that Bioconductor package download
#' stats are not version-specific.
#'
#' @param pkgType character(1) Either one of 'software', 'data-experiment',
#'     'workflows', or 'data-annotation' (defaults to 'all' or 'software')
#'
#' @importFrom dplyr mutate %>%
#' @importFrom utils read.table
#' @importFrom tibble as_tibble
#'
#' @return A \code{data.frame} of download statistics for
#' all Bioconductor packages, in tidy format
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
    linkPkg <- gsub("software", "bioc", pkgType)
    fnameType <- gsub("data-", "", linkPkg)

    base_url <- "http://bioconductor.org/packages/stats/%s/%s_pkg_stats.tab"

    stats_urls <- sprintf(base_url,
        linkPkg, fnameType
    )

    tlist <- lapply(stats_urls, read.table, header = TRUE)
    tbl <- as_tibble(
        cbind(
            pkgType = rep(pkgType, vapply(tlist, nrow, numeric(1L))),
            dplyr::bind_rows(tlist)
        )
    )

    tbl <- filter(tbl, .data$Month != "all") %>%
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
    pkgType <- switch(pkgType, software = "bioc", pkgType)

    base_url <- "http://bioconductor.org/packages/stats/%s/%s/%s_%s_stats.tab"

    stats_urls <- sprintf(base_url,
        pkgType, pkg, pkg, years
    )

    tlist <- lapply(stats_urls, .try.read.table, header = TRUE)
    tbl <- as_tibble(dplyr::bind_rows(tlist))
    filter(tbl, .data$Month != "all")

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
firstInBioc = function(download_stats) {
  download_stats %>%
    dplyr::filter(.data$Month!='all') %>%
    dplyr::group_by(.data$Package) %>%
    # thanks: https://stackoverflow.com/questions/43832434/arrange-within-a-group-with-dplyr
    dplyr::top_n(1, dplyr::desc(.data$Date)) %>%
    dplyr::collect()
}
