#' Tidy Bioconductor build report results
#'
#' The online Bioconductor build reports
#' are great for humans to look at, but
#' they are not easily computable. This function
#' scrapes HTML and text files available
#' from the build report online pages to generate
#' a tidy data frame version of the build report.
#'
#' @param version character(1) the character version number
#' as used to access the online build report. For
#' example, "3.14". The default is the "current version"
#' as given by \code{BiocManager::version()}. Note
#' that this is a character vector of length one and not a number.
#'
#' @param stage.timings logical(1) Whether to include the start, end, and
#' elapsed time for each build, check, install stage from each building in
#' the result (default: FALSE)
#'
#' @inheritParams biocBuildStatusDB
#'
#' @return A \code{tbl_df} object with columns pkg, version,
#' author, commit, date, node, stage, and result.
#'
#' @importFrom readr read_lines
#' @importFrom tibble as_tibble
#' @importFrom rvest html_text html_text2 html_nodes
#' @importFrom xml2 read_html
#' @importFrom dplyr left_join
#' @importFrom BiocManager version
#'
#' @examples
#'
#' # Set the stage--what version of Bioc am I using?
#' BiocManager::version()
#'
#' latest_build <- biocBuildReport()
#' head(latest_build)
#'
#' @export
biocBuildReport <- function(
    version = BiocManager::version(),
    pkgType = c(
        "software", "data-experiment", "data-annotation", "workflows"
    ),
    stage.timings = FALSE
) {
  stopifnot(is.logical(stage.timings), is.character(pkgType))
  if (version %in% c("release", "devel"))
    version <- BiocManager:::.version_bioc(version)

  z <- biocBuildStatusDB(version, pkgType)

  if (version >= package_version("3.14")) {
    y <- biocBuildReportDB(version, pkgType)

    if (stage.timings) {
      typeTimings <- lapply(attr(y, "dcf_folder"), .read_summary_dcfs)
      timings <- do.call(rbind, unname(typeTimings))
      z <- merge(
          z, timings,
          by.x = c("pkg", "node", "stage"),
          by.y = c("Package", "node", "stage")
      )
    }
  } else {
    warning("Only 'software' provided for Bioconductor < 3.14")
    url <- .get_build_report_url(version, pkgType = "bioc")
    dat <- xml2::read_html(url)

    rowspan <- length(html_text(html_nodes(
        dat,xpath='/html/body/table[@class="node_specs"]/tr[@class!=""]'
    )))
    if(rowspan > 5L || rowspan < 2L){
      warning("Detected an unusual number of builders == ",rowspan," ... ")
    }
    res <- html_nodes(dat,
      xpath = '/html/body/table[@id="THE_BIG_GCARD_LIST"]') |>
      html_nodes("tr") |> html_nodes("td") |> html_nodes("b") |>
      html_nodes("a")
    pkgnames <- html_text(res)
    versions <- html_nodes(dat,
      xpath = '/html/body/table[@id="THE_BIG_GCARD_LIST"]') |>
      html_nodes(xpath = "//td[@rowspan=3]") |> html_nodes("b") |>
      html_text2()
    versions <- vapply(strsplit(versions, "\\s"), `[`, character(1L), 2L)
    maints <- html_nodes(dat,
      xpath = '//*[@id="THE_BIG_GCARD_LIST"]/tbody/tr/td[@rowspan=3]/text()'
    ) |> html_text2()
    # Account for packages with malformed maintainer fields in page
    idx <- which(rle(maints)$lengths > 1)
    if (length(idx)) {
        off_set <- seq(0, length(idx) - 1)
        idx <- idx + off_set
        for (i in off_set) {
          maints <- append(maints, values = " ", after = idx[i+1] + 1)
        }
    }
    maints <- maints[c(FALSE, TRUE)]
    stopifnot(identical(length(pkgnames), length(maints)))

    meta <- html_nodes(dat,
      xpath = '//*[@id="THE_BIG_GCARD_LIST"]/tbody/tr/td[@rowspan=3]')
    values <- meta |> html_nodes("table") |> html_text2()

    if (version >= package_version("3.13")) {
      values <- trimws(gsub("[\n]*git_last_commit[_date]*:", "", values))
      splitter <- "\\s"
    } else {
      values <- trimws(gsub("Last.Commit:|.Last.Changed.Date", "", values))
      splitter <- ": "
    }

    commitdate <- do.call(rbind.data.frame, strsplit(values, splitter))
    names(commitdate)[1:2] <- c("git_last_commit", "git_last_commit_date")
    commitdate[["git_last_commit_date"]] <-
        as.POSIXct(commitdate[["git_last_commit_date"]])

    y <- data.frame(
      pkg = pkgnames,
      author = maints,
      version = versions,
      commitdate[, 1:2]
    )
    y <- y[!is.na(y$pkg),]
  }

  depdf <- get_deprecated_status_df(version)
  isEmpty <- all(
      vapply(depdf, function(x) identical(length(x), 0L), logical(1L))
  )
  if (!isEmpty)
      y <- merge(y, depdf, by.x = "pkg", by.y = "Package")

  df <- suppressMessages(left_join(y, z)) # just suppress "Joining by...."
  df <- as_tibble(df)
  if (!nrow(df)) {
    warning("No Bioconductor build report found.")
    return(df)
  }
  df[['bioc_version']] <- as.character(version)
  attr(df,'git_last_commit_date') <- as.POSIXct(df[['git_last_commit_date']][1])
  attr(df,'class') = c('biocBuildReport',class(df))
  df
}
