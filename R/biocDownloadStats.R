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

.get_all_biocpkgs <- function() {
    db <- available.packages(
        repos = BiocManager:::.repositories_bioc(version = version)
    )
    rownames(db)
}

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
#'     bfcquery bfcnew bfcdownload
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
    repo_name <- .matchGetShortName(pkgType, "repo.name")
    if (identical(pkgType, "all"))
        pkgs <- .get_all_biocpkgs()
    else
        pkgs <- biocPkgList(repo = repo_name)[["Package"]]

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

    tbl <- tbl[tbl$Package %in% pkgs, ]

    class(tbl) <- c('bioc_downloads', class(tbl))
    tbl
}

.try.read.table <- function(...) {
    withCallingHandlers({
        tryCatch({
            read.table(...)
        }, error = function(err) {
            data.frame(
                Year = integer(0L), Month = character(0L),
                Nb_of_distinct_IPs = integer(0L), Nb_of_downloads = integer(0L)
            )
        })
    }, warning = function(w) {
        w <- conditionMessage(w)
        if (grepl("404 Not Found", w, fixed = TRUE))
            warning(
                "No data for year: ", gsub(".*_([0-9]{4})_.*", "\\1", w),
                call. = FALSE
            )
        invokeRestart("muffleWarning")
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
    viewsdb <- get_VIEWS(version = version, repoType)

    pct <- round(rank*100 / nrow(viewsdb), 2)
    names(pct) <- paste(rank, nrow(viewsdb), sep = "/")
    pct
}

#' Summary of the latest package statistics
#'
#' The `latestPkgStats` function combines outputs from several functions to
#' generate a table of relevant statistics for a given package.
#'
#' @inheritParams pkgDownloadRank
#' @inheritParams activitySince
#'
#' @examples
#' if (interactive()) {
#'
#'   latestPkgStats("Bioconductor/BiocGenerics", "2021-05-05")
#'
#' }
#' @export
latestPkgStats <-
    function(
        gh_repo,
        Date,
        pkgType = c(
            "software", "data-experiment", "workflows", "data-annotation"
        )
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
    csince <- nrow(activitySince(gh_repo, "commits", Date = Date))
    stattab[afterDate, ] |>
      dplyr::summarize(
          avg_mo_distict_IPs = mean(Nb_of_distinct_IPs),
          avg_mo_downloads = mean(Nb_of_downloads)
      ) |>
      dplyr::mutate(Package = pkg, .before = 1, Date = Date) |>
      dplyr::mutate(
          downloadRank = pkgDownloadRank(pkg, pkgType),
          issuesClosedSince =
              nrow(activitySince(gh_repo, "issues", "closed", Date)),
          commitsSince = csince
      )
}

.commit_table <- function(jsonlist) {
    parents <- jsonlist[["parents"]][["sha"]]
    if (is.null(parents)) parents <- NA_character_
    cdata <- jsonlist[["commit"]]
    author <- cdata[["author"]][["name"]]
    commit <- cdata[["tree"]][["sha"]]
    committer_date <- cdata[["committer"]][["date"]]
    message <- cdata[["message"]]
    tibble::tibble(committer_date, commit, parents, author, message)
}

.gh_data_process <- function(x, fields, commits = FALSE) {
    if (commits)
      .commit_table(x)
    else
      x[fields]
}

#' What are the issues, pulls, commits created since a date?
#'
#' This function uses the `gh` package to get a list of either issues, pull
#' requests, or GitHub commits since the specified date for a particular GitHub
#' repository. The repository must have both the username / organization and the
#' name, e.g., "Bioconductor/S4Vectors".
#'
#' @details The `tibble` returned by the commits activity report contains five
#'   columns:
#'   * 'committer_date'
#'   * 'commit' - hash
#'   * 'parents' - hash of parent for merge commits
#'   * 'author'
#'   * 'message'
#'
#'   For information on other columns, refer to the GitHub API under repository
#'   issues or pulls (e.g., `/repos/:repo/issues`).
#'
#' @param gh_repo character(1) The GitHub repository location including the
#'   username / organization and the repository name, e.g.,
#'   "Bioconductor/S4Vectors"
#'
#' @param activity character(1) The type of repository activity to pull from the
#'   GitHub API. It can be one of "issues" (default), "pulls", or "commits".
#'
#' @param Date character(1) The date cutoff from which to analyze closed issues
#'   in the YYYY-MM-DD or YYYY-MM-DDTHH:MM:SSZ format (ISO 8601).
#'
#' @param status character(1) One of 'closed', 'open', or 'all' corresponding to
#'   the issue state desired from the GitHub API (Default: "closed"). This
#'   argument is ignored for the "commits" activity report.
#'
#' @param issue_metadata character() The metadata labels to extract from the
#'   `gh::gh` response. See `?gh::gh` for more details. Defaults to
#'   'created_at', 'number', and 'title'. This argument is ignored for the
#'   "commits" activity report.
#'
#' @param token character(1) For big requests, e.g., commit history, you may be
#'   prompted to use a GitHub Personal Access Token. Enter the token as plain
#'   text.
#'
#' @return A `tibble` with three columns corresponding to issue metadata (i.e.,
#'   "created_at", "number", "title")
#'
#' @examples
#' if (interactive()) {
#'
#'   activitySince("Bioconductor/S4Vectors", "issues", "closed", "2021-05-01")
#'   activitySince("Bioconductor/S4Vectors", "issues", "open", "2022-05-01")
#'   activitySince("Bioconductor/S4Vectors", "commits", Date = "2022-05-01")
#'
#' }
#'
#' @export
activitySince <- function(
    gh_repo,
    activity = c("issues", "pulls", "commits"),
    status = c("closed", "open", "all"),
    Date,
    issue_metadata = c("created_at", "number", "title"),
    token = NULL
) {
    activity <- match.arg(activity)
    isCommits <- identical(activity, "commits")
    if (!isCommits)
      status <- match.arg(status)
    else
      status <- NULL
    if (!requireNamespace("lubridate", quietly = TRUE))
        stop("Install the 'lubridate' package to work with dates")
    accept <- "application/vnd.github.v3+json"
    act_report <- gh(
        endpoint = paste0("/repos/:repo/", activity),
        .token = token,
        repo = gh_repo,
        .limit = Inf,
        state = status,
        since = Date,
        .accept = accept
    )
    dplyr::bind_rows(
        lapply(act_report, .gh_data_process,
            fields = issue_metadata, commits = isCommits)
    )
}
