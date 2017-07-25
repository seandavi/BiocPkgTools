#' get bioconductor download stats
#'
#' @details Note that bioconductor package download
#' stats are not version-specific.
#'
#' @return a \code{data.frame} of download stats for
#' all bioconductor packages, in tidy format
#'
#' @export
getBiocDownloadStats = function() {
  tmp = read.table('http://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab',
                   sep="\t", header = TRUE)
  tmp$repo = 'Software'
  tmp2 = read.table('http://bioconductor.org/packages/stats/data-annotation/annotation_pkg_stats.tab',
                    sep="\t", header = TRUE)
  tmp2$repo = 'AnnotationData'
  tmp3 = read.table('http://bioconductor.org/packages/stats/data-experiment/experiment_pkg_stats.tab',
                    sep="\t", header = TRUE)
  tmp3$repo = 'ExperimentData'
  return(rbind(tmp,tmp2,tmp3))
}
