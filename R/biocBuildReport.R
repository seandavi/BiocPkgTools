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
#' @import rex
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
biocBuildReport <- function(version=BiocManager::version()) {
  requireNamespace("rex")
  url <- sprintf('https://bioconductor.org/checkResults/%s/bioc-LATEST/STATUS_DB.txt',version)
  dat <- readr::read_lines(url)
  z <- re_matches(dat,rex(
    start,
    capture(except_any_of('#'),name='pkg'),
    '#',
    capture(except_any_of('#'),name='node'),
    '#',
    capture(except_any_of(blank_pcre_utf8,':'),name='stage'),
    ':',blank_pcre_utf8,
    capture(anything,name='result')
  ))


  dat <- xml2::read_html(sprintf('https://bioconductor.org/checkResults/%s/bioc-LATEST/',version))

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
  # scRepertoire has a malformed DESCRIPTION file (breaking this code)
  maints <- html_nodes(dat,
    xpath = '//*[@id="THE_BIG_GCARD_LIST"]/tbody/tr/td[@rowspan=3]/text()') %>%
    html_text2()
  # temporary until fixed
  maints <- Filter(function(x) nchar(x) > 1, maints)
  maints <- append(maints, NA, which(pkgnames == "scRepertoire"))
  # maints <- maints[c(FALSE, TRUE)]

  meta <- html_nodes(dat,
    xpath = '//*[@id="THE_BIG_GCARD_LIST"]/tbody/tr/td[@rowspan=3]')
  values <- meta %>% html_nodes("table") %>% html_text2()
  values <- trimws(gsub("Last.Commit:|.Last.Changed.Date", "", values))
  commitdate <- unlist(strsplit(values, ": "))
  commits <- commitdate[c(TRUE, FALSE)]
  dates <- commitdate[!c(TRUE, FALSE)]

  y <- data.frame(
    pkg = pkgnames,
    author = maints,
    version = versions,
    last_commit = commits,
    last_changed_date = as.POSIXct(dates)
  )
  y <- y[!is.na(y$pkg),]

  df <- suppressMessages(left_join(y, z)) # just suppress "Joining by...."
  df <- as_tibble(df)
  if (!nrow(df)) {
    warning("No Bioconductor build report found.")
    return(df)
  }
  df[['bioc_version']] <- as.character(version)
  attr(df,'last_changed_date') <- as.POSIXct(df[['last_changed_date']][1])
  attr(df,'class') = c('biocBuildReport',class(df))
  df
}

blank_pcre_utf8 <- rex:::character_class("\\p{Zs}")
