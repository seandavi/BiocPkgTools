#' Tidy Bioconductor build report results
#'
#' The online Bioconoductor build reports
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
#' @return A \code{tbl_df} object with columns pkg, version,
#' author, commit, date, node, stage, and result.
#'
#' @importFrom readr read_lines
#' @importFrom tibble as_tibble
#' @importFrom rvest html_text html_text2 html_nodes
#' @importFrom xml2 read_html
#' @importFrom dplyr left_join
#' @importFrom BiocManager version
#' @importFrom magrittr %>%
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
biocBuildReport <- function(version=BiocManager::version(), stage.timings = FALSE) {
  if (version %in% c("release", "devel"))
    version <- BiocManager:::.version_bioc(version)

  url <- get_build_status_db_url(version)
  dat <- readLines(url)
  z <- do.call(rbind.data.frame, strsplit(dat, "#|: "))
  names(z) <- c("pkg", "node", "stage", "result")

  if (version >= package_version("3.14")) {
    tfile <- paste(dirname(url), "report.tgz", sep = "/")
    treport <- .cache_url_file(tfile)
    untar(treport, exdir = dcf_folder <- tempfile())
    y <- .read_info_dcfs(dcf_folder)

    if (stage.timings)
      z <- merge(z, .read_summary_dcfs(dcf_folder),
          by.x = c("pkg", "node", "stage"),
          by.y = c("Package", "node", "stage")
      )
  } else {
    dat <- xml2::read_html(dirname(url))

    rowspan <- length(html_text(html_nodes(
        dat,xpath='/html/body/table[@class="node_specs"]/tr[@class!=""]'
    )))
    if(rowspan > 5L || rowspan < 2L){
      warning("Detected an unusual number of builders == ",rowspan," ... ")
    }
    res <- html_nodes(dat,
      xpath = '/html/body/table[@id="THE_BIG_GCARD_LIST"]') %>%
      html_nodes("tr") %>% html_nodes("td") %>% html_nodes("b") %>%
      html_nodes("a")
    pkgnames <- html_text(res)
    versions <- html_nodes(dat,
      xpath = '/html/body/table[@id="THE_BIG_GCARD_LIST"]') %>%
      html_nodes(xpath = "//td[@rowspan=3]") %>% html_nodes("b") %>%
      html_text2()
    versions <- vapply(strsplit(versions, "\\s"), `[`, character(1L), 2L)
    maints <- html_nodes(dat,
      xpath = '//*[@id="THE_BIG_GCARD_LIST"]/tbody/tr/td[@rowspan=3]/text()'
    ) %>% html_text2()
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
    values <- meta %>% html_nodes("table") %>% html_text2()

    if (version >= package_version(3.13)) {
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

get_build_status_db_url <- function(version) {
  db_prefix <- if (package_version(version) >= '3.13') "BUILD_" else ""
  db_file <- paste0(db_prefix, "STATUS_DB.txt")
  sprintf(
    'https://bioconductor.org/checkResults/%s/bioc-LATEST/%s', version, db_file
  )
}

get_deprecated_status_db_url <- function(version) {
  sprintf(
    "https://bioconductor.org/checkResults/%s/bioc-LATEST/meat-index.dcf",
    version
  )
}

get_deprecated_status_df <- function(version) {
    statusIndexFile <- file(get_deprecated_status_db_url(version))
    pkg_status <- try(read.dcf(statusIndexFile))
    close(statusIndexFile)
    if (!inherits(pkg_status, "try-error")) {
        depdf <- cbind.data.frame(
            Package = pkg_status[, "Package"],
            Deprecated = pkg_status[, "PackageStatus"] == "Deprecated" &
                !is.na(pkg_status[, "PackageStatus"]),
            PackageStatus = pkg_status[, "PackageStatus"]
        )
    } else {
        depdf <- data.frame(
            Package = character(0L),
            Deprecated = logical(0L),
            PackageStatus = character(0L)
        )
    }
    depdf
}

.read_summary_dcfs <- function(dcf_location) {
    summary_dcfs <- list.files(dcf_location, pattern="-summary\\.dcf$",
        full.names = TRUE, recursive = TRUE)
    fields <- c("Package", "StartedAt", "EndedAt", "EllapsedTime")
    summaries <- lapply(summary_dcfs, .import_dcf_stage_node, fields = fields)
    do.call(rbind, summaries)
}

.import_dcf_stage_node <- function(filepath, fields) {
    stage <- head(strsplit(basename(filepath), "-", fixed = TRUE)[[1L]], 1L)
    node <- basename(dirname(filepath))
    dcf_pkg <- read.dcf(filepath, fields = fields)
    dcf_chr <- structure(as.character(dcf_pkg), .Names = fields)
    append(dcf_chr, c(node = node, stage = stage), after = 1L)
}

.read_info_dcfs <- function(info_files_location) {
    dcffiles <- list.files(path = info_files_location,
        pattern = "info\\.dcf$", full.names = TRUE, recursive = TRUE)
    meta <- do.call(rbind.data.frame, lapply(dcffiles, read.dcf))
    y <- meta[,
        c("Package", "Maintainer", "Version",
        "git_last_commit", "git_last_commit_date")
    ]
    y[["git_last_commit_date"]] <-
        as.POSIXct(gsub("^(.*)\\s\\(.*", "\\1", y[["git_last_commit_date"]]))
    names(y)[1:3] <- c("pkg", "author", "version")
    y
}

.cache_url_file <- function(url) {
    bfc <- BiocFileCache()
    bquery <- bfcquery(bfc, url, "rname", exact = TRUE)
    if (identical(nrow(bquery), 1L) && bfcneedsupdate(bfc, bquery[["rid"]]))
        bfcupdate(bfc, bquery[["rid"]], url)

    bfcrpath(
        bfc, rnames = url, exact = TRUE, download = TRUE, rtype = "web"
    )
}
