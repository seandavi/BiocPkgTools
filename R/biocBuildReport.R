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
#' @return a \code{tbl_df} object with columns pkg, version,
#' author, commit, date, node, stage, and result.
#' 
#' @importFrom readr read_lines
#' @importFrom tibble as_tibble
#' @importFrom rvest html_text html_nodes
#' @importFrom xml2 read_html
#' @import rex
#' @importFrom dplyr left_join
#' @importFrom BiocManager version
#' 
#' @examples 
#' 
#' # Set the stage--what version of Bioc am 
#' # I using?
#' BiocManager::version()
#' 
#' latest_build = biocBuildReport()
#' head(latest_build)
#' 
#' @export
biocBuildReport <- function(version=as.character(BiocManager::version())) {
  requireNamespace("rex")
    if(!is.character(version)) {
        stop('version should be a character string representing the Bioconductor version, such as "3.6"')
    }
  url = sprintf('http://bioconductor.org/checkResults/%s/bioc-LATEST/STATUS_DB.txt',version)
  dat = readr::read_lines(url)
  z = re_matches(dat,rex(
    start,
    capture(except_any_of('#'),name='pkg'),
    '#',
    capture(except_any_of('#'),name='node'),
    '#',
    capture(except_any_of(blank_pcre_utf8,':'),name='stage'),
    ':',blank_pcre_utf8,
    capture(anything,name='result')
  ))
  
  
  dat = xml2::read_html(sprintf('http://bioconductor.org/checkResults/%s/bioc-LATEST/',version))
  
  rowspan = length(html_text(html_nodes(dat,xpath='/html/body/table[@class="node_specs"]/tr[@class!=""]')))
  if(rowspan > 5L || rowspan < 2L){
    warning("Detected an unusual number of builders == ",rowspan," ... ")
  }
  pkgnames = html_text(html_nodes(dat,xpath=sprintf('/html/body/table[@class="mainrep"]/tr/td[@rowspan="%s"]',rowspan)))
  
  y = re_matches(pkgnames,
                 rex(
                   start,
                   # matches .standard_regexps()$valid_package_name
                   capture(alpha,any_of(alnum,"."),alnum, name = "pkg"),
                   maybe(blank_pcre_utf8),
                   # matches .standard_regexps()$valid_package_version
                   capture(between(group(digits,character_class(".-")),1,""),digits, name = "version"),
                   maybe(blank_pcre_utf8),
                   capture(anything,name='author'),
                   "Last",anything,"Commit:",
                   capture(anything,name="commit"),
                   "Last",anything,'Changed',anything,"Date:",any_non_alnums,
                   capture(any_of(list(digit,'-',blank_pcre_utf8,':')),name='last_changed_date')
                 ))
  y = y[!is.na(y$pkg),]
  
  df = suppressMessages(y %>% left_join(z)) # just suppress "Joining by...."
  df = as_tibble(df)
  if(nrow(df) == 0){
    warning("No Bioconductor build report found.")
    return(df)
  }
  df[['bioc_version']]=version
  df[['last_changed_date']] = as.POSIXct(df[['last_changed_date']])
  attr(df,'last_changed_date') = as.POSIXct(df[['last_changed_date']][1])
  attr(df,'class') = c('biocBuildReport',class(df))
  df
}

blank_pcre_utf8 <- rex:::character_class("\\p{Zs}")
