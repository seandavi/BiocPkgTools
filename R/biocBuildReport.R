.isBiocVersion <- function(version, keyword) {
    identical(package_version(version), BiocManager:::.version_bioc(keyword))
}

#' Tidy Bioconductor build report results
#'
#' The online Bioconoductor build reports
#' are great for humans to look at, but
#' they are not easily computable. This function
#' scrapes HTML and text files available
#' from the build report online pages to generate
#' a tidy data frame version of the build report.
#'
#' @param version character(1) the version number
#' as used to access the online build report. For
#' example, "3.6". The default is the "current version"
#' as specified in \code{BiocManager::version}. Note
#' that this is a character() variable, not a number.
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
biocBuildReport <- function(version=BiocManager::version()) {
  url <- get_build_status_db_url(version)
  dat <- readLines(url)
  z <- do.call(rbind.data.frame, strsplit(dat, "#|: "))
  names(z) <- c("pkg", "node", "stage", "result")

  if (.isBiocVersion(version, "devel")) {
    tfile <- paste(dirname(url), "report.tgz", sep = "/")
    download.file(tfile, treport <- tempfile(fileext = ".tgz"))
    report_files <- untar(treport, list = TRUE)
    dcffiles <- grep("info\\.dcf$", report_files, value = TRUE)
    dcfs <- untar(treport, files = dcffiles, exdir = dcf_folder <- tempfile())
    full_dcfs <- list.files(dcf_folder, recursive = TRUE, full.names = TRUE)
    meta <- do.call(rbind.data.frame, lapply(full_dcfs, read.dcf))
    y <- meta[,
        c("Package", "Maintainer", "Version",
        "git_last_commit", "git_last_commit_date")
    ]
    y[["git_last_commit_date"]] <-
        as.POSIXct(gsub("^(.*)\\s\\(.*", "\\1", y[["git_last_commit_date"]]))
    names(y)[1:3] <- c("pkg", "author", "version")
  } else {
  dat <- xml2::read_html(dirname(url))

  rowspan <- length(html_text(html_nodes(dat,xpath='/html/body/table[@class="node_specs"]/tr[@class!=""]')))
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
    html_nodes(xpath = "//td[@rowspan=3]") %>% html_nodes("b") %>% html_text2()
  versions <- vapply(strsplit(versions, "\\s"), `[`, character(1L), 2L)
  maints <- html_nodes(dat,
    xpath = '//*[@id="THE_BIG_GCARD_LIST"]/tbody/tr/td[@rowspan=3]/text()') %>%
    html_text2()
  # Account for packages with malformed maintainer fields in page
  idx <- which(rle(maints)$lengths > 1)
  off_set <- seq(0, length(idx) - 1)
  idx <- idx + off_set
  for (i in off_set) {
    maints <- append(maints, values = " ", after = idx[i+1] + 1)
  }
  maints <- maints[c(FALSE, TRUE)]
  stopifnot(identical(length(pkgnames), length(maints)))

  meta <- html_nodes(dat,
    xpath = '//*[@id="THE_BIG_GCARD_LIST"]/tbody/tr/td[@rowspan=3]')
  values <- meta %>% html_nodes("table") %>% html_text2()

  if (.isBiocVersion(version, "devel")) {
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
  sprintf('https://bioconductor.org/checkResults/%s/bioc-LATEST/%s',version, db_file)
}

