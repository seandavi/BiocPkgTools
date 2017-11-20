#' Tidy Bioconductor build report results
#' 
#' The online Bioconoductor build reports
#' are great for humans to look at, but
#' they are not easily computable. This function
#' simply scrapes HTML and text files available
#' from the build report online pages to generate
#' a tidy data frame version of the build report.
#' 
#' @param version character(1) the version number
#' as used to access the online build report. For 
#' example, "3.6".
#' 
#' @return a data.frame with columns pkg, version,
#' author, commit, date, node, stage, and result.
#' 
#' @importFrom readr read_lines
#' @importFrom rvest html_text html_nodes
#' @importFrom xml2 read_html
#' @import rex
#' @importFrom dplyr left_join
#' @importFrom BiocInstaller biocVersion
#' 
#' @examples 
#' 
#' latest_build = build_report("3.6")
#' head(latest_build)
#' 
#' @export
build_report <- function(version=biocVersion()) {
  url = sprintf('http://bioconductor.org/checkResults/%s/bioc-LATEST/STATUS_DB.txt',version)
  dat = readr::read_lines(url)
  z = re_matches(dat,rex(
    start,
    capture(except_any_of('#'),name='pkg'),
    '#',
    capture(except_any_of('#'),name='node'),
    '#',
    capture(except_any_of(blank,':'),name='stage'),
    ':',blank,
    capture(anything,name='result')
  ))
  
  
  dat = xml2::read_html(sprintf('http://bioconductor.org/checkResults/%s/bioc-LATEST/',version))
  
  pkgnames = html_text(html_nodes(dat,xpath='/html/body/table[@class="mainrep"]/tr/td[@rowspan="3"]'))
  
  y = rex::re_matches(pkgnames,
                      rex(
                        start,
                        capture(any_alnums, name='pkg'),
                        maybe(any_blanks),
                        capture(except_any_of(any_alphas),name="version"),
                        maybe(any_blanks),
                        capture(anything,name='author'),
                        "Last",anything,"Commit:",
                        capture(anything,name="commit"),
                        "Last",anything,'Changed',anything,"Date:",any_non_alnums,
                        capture(any_of(list(digit,'-',blank,':')),name='last_changed_date')
                      ))
  y = y[!is.na(y$pkg),]
  
  library(dplyr)
  df = y %>% left_join(z)
  df[['last_changed_date']] = as.POSIXct(df[['last_changed_date']])
  attr(df,'last_changed_date') = as.POSIXct(df[['last_changed_date']][1])
  df
}

