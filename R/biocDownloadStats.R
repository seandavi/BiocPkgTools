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

#' What is a package's download rank?
#'
#' This function uses `available.packages` to calculate the download rank
#' _percentile_ of a given package. It approximates what is observed
#' in the Bioconductor landing page.
#'
#' @inheritParams pkgDownloadStats
#' @inheritParams repositoryStats
#'
#' @return The package's percentile rank, in terms of download statistics, and
#'   proportion in the name
#'
#' @examples
#'
#' ## Percentile rank for BiocGenerics (top 1%)
#' pkgDownloadRank("BiocGenerics", "software")
#'
#' @export
pkgDownloadRank <-
    function(
        pkg,
        pkgType = c(
            "software", "data-experiment", "workflows", "data-annotation"
        ),
        version = BiocManager::version()
    )
{
    pkgType <- match.arg(pkgType)
    jsonPath <- .matchGetShortName(pkgType, "json.file")
    rankURL <-
        "https://www.bioconductor.org/packages/json/%s/%s/packages.json"
    ufile <- .cache_url_file(sprintf(rankURL, version, jsonPath))

    pkgs <- jsonlite::fromJSON(ufile)
    rank <- pkgs[[pkg]]$Rank

    repoType <- .matchGetShortName(pkgType, "repo.name")
    repos <- BiocManager:::.repositories_bioc(version)[repoType]
    db <- available.packages(repos = repos)
    pct <- round(rank*100 / nrow(db), 2)
    names(pct) <- paste(rank, nrow(db), sep = "/")
    pct
}

#' Summary of the latest package statistics
#'
#' The `latestPkgStats` function combines outputs from several functions to
#' generate a table of relevant statistics for a given package.
#'
#' @inheritParams pkgDownloadRank
#' @inheritParams issuesSince
#' @inheritParams commitsSince
#'
#' @examples
#'
#' latestPkgStats("Bioconductor/BiocGenerics", "2021-05-05")
#'
#' @export
latestPkgStats <-
    function(
        gh_repo,
        Date,
        pkgType = c(
            "software", "data-experiment", "workflows", "data-annotation"
        ),
        local_repo
    )
{
    if (!requireNamespace("lubridate", quietly = TRUE))
        stop("Install the 'lubridate' package to work with dates")
    y1 <- lubridate::year(Date)
    y2 <- lubridate::year(Sys.Date())
    m1 <- lubridate::month(Date, label = TRUE, abbr = TRUE)

    pkg <- strsplit(gh_repo, "/", fixed = TRUE)[[1]][[2]]
    stattab <- pkgDownloadStats(pkg, years = y1:y2)
    afterDate <- seq_len(nrow(stattab)) >=
      with(stattab, which(Year == y1 & Month == m1))
    csince <- NA_integer_
    if (!missing(local_repo))
        csince <- nrow(commitsSince(local_repo, Date))
    stattab |>
      dplyr::summarize(
          avg_mo_distict_IPs = mean(Nb_of_distinct_IPs),
          avg_mo_downloads = mean(Nb_of_downloads)
      ) |>
      dplyr::mutate(Package = pkg, .before = 1, Date = Date) |>
      dplyr::mutate(
          downloadRank = pkgDownloadRank(pkg, pkgType),
          issuesClosedSince = nrow(issuesSince(gh_repo, Date)),
          commitsSince = csince
      )
}

#' What are the issues created since a date?
#'
#' This function uses the `gh` package to get a list of issues since the
#' specified date for a particular GitHub repository. The repository must
#' have both the username / organization and the name, e.g.,
#' "Bioconductor/S4Vectors".
#'
#' @param gh_repo character(1) The GitHub repository location including the
#'   username / organization and the repository name, e.g.,
#'   "Bioconductor/S4Vectors"
#'
#' @param Date character(1) The date cutoff from which to analyze closed issues
#'   in the year-month-day format.
#'
#' @param status character(1) One of 'closed', 'open', or 'all' corresponding to
#'   the issue state desired from the GitHub API (Default: "closed")
#'
#' @param issue_metadata character() The metadata labels to extract from the
#'   `gh::gh` response. See `?gh::gh` for more details. Defaults to
#'   'created_at', 'number', and 'title'.
#'
#' @return A `tibble` with three columns corresponding to issue metadata (i.e.,
#'   "created_at", "number", "title")
#'
#' @examples
#'
#' issuesSince("Bioconductor/S4Vectors", "2021-05-01", "closed")
#'
#' @export
issuesSince <- function(
    gh_repo,
    Date,
    status = c("closed", "open", "all"),
    issue_metadata = c("created_at", "number", "title")
) {
    status <- match.arg(status)
    if (!requireNamespace("lubridate", quietly = TRUE))
        stop("Install the 'lubridate' package to work with dates")
    accept <- "application/vnd.github.v3+json"
    issues <- gh("/repos/:repo/issues", repo = gh_repo, .limit = Inf,
        state = status, .accept = accept)
    issuedf <- dplyr::bind_rows(
        lapply(issues, function(issue) {
            issue[c("created_at", "number", "title")]
        })
    )
    issuedf[lubridate::ymd_hms(issuedf[["created_at"]]) >= Date, ]
}

#' How many commits have created since a date?
#'
#' The `commitsSince` function allows maintainers to count the number of commits
#' since a given date using the `git log` command.
#'
#' @details The `tibble` given by the function has 5 columns, 'datetime',
#'   'commit', 'parents', 'author', and 'subject'. The 'parents' column refers
#'   to the parent commit.
#'
#' @param local_repo character(1) The path to a local git repository for which
#'   to review the commit history
#'
#' @param exclude_author character() A vector of author names to exclude from
#'   the results
#'
#' @inheritParams issuesSince
#'
#' @return A `tibble` with commit history from the `git log` command
#'
#' @examples
#' if (interactive()) {
#'     commitsSince("~/bioc/S4Vectors", "2021-05-05")
#' }
#'
#' @export
commitsSince <- function(local_repo, Date, exclude_author) {
    git_cmd <- paste("git -C", local_repo, "log",
        "--pretty=format:'%cd\t%h\t%p\t%an\t%s'",
        "--date=format:'%Y-%m-%d'",
        paste0("--after=", shQuote(Date))
    )
    reslog <- system(git_cmd, intern = TRUE)
    restab <- read.delim(text = paste0(reslog, collapse = "\n"), sep = "\t")
    names(restab) <- c("datetime","commit", "parents", "author", "subject")
    tibble::as_tibble(restab)
}
