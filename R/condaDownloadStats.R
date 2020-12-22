#' Get download statistics for Bioconductor packages distributed via
#' Anaconda.
#'
#' @details Anaconda provide daily download counts for all software packages
#' they distribute.  The are summarised into monthly tables of counts and made 
#' available from https://github.com/grimbough/anaconda-download-stats
#' This function provides a mechanism to download these monthly counts for 
#' Bioconductor packages distributed through Anaconda.#' 
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom utils read.table download.file
#' @importFrom tibble as_tibble
#'
#' @return A \code{data.frame} of download statistics for
#' all Bioconductor packages distributed by Anaconda, in tidy format.
#' Note: Anaconda do not provide counts for unique IP addresses.  This column
#' is listed as \code{NA} for all packages to provide continuity with data from
#' Bioconductor.org obtained by \code{\link{biocDownloadStats()}}
#'
#' @examples
#' anacondaDownloadStats()
#'
#' @export
anacondaDownloadStats = function() {
  temp_file = tempfile()
  url = 'https://github.com/grimbough/anaconda-download-stats/blob/master/rdata/bioc_counts.rds?raw=true'
  download.file(url, destfile = temp_file, quiet = TRUE)
  
  tmp = readRDS(temp_file)
  tmp$repo = 'Anaconda'
  tmp$Nb_of_distinct_IPs = NA_integer_

  tmp = as_tibble(tmp) %>%
    dplyr::mutate(Date = as.Date(paste(.data$Year, .data$Month, '01'),
                                 '%Y %b %d')) %>%
    select('Package', 'Year', 'Month', 'Nb_of_distinct_IPs', 'Nb_of_downloads', 'repo', 'Date')
  
  class(tmp) = c('bioc_downloads', class(tmp))
  tmp
}
