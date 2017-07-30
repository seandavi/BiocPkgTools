#' Get full bioc software package listing
#'
#' The BiocViews-generated \code{VIEWS} file is available
#' for bioconductor release and devel repositories. It
#' contains quite a bit more information from the
#' package \code{DESCRIPTION} files than the \code{PACKAGES}
#' file. In particular, it contains \code{biocViews} annotations
#' and URLs for vignettes and developer URLs.
#'
#' @param version The requested bioconductor version. Will
#'     default to use the BiocInstaller defaults (ie., \code{biocVersion()}).
#'
#' @return a \code{data.frame}
#'
#' @importFrom BiocInstaller biocinstallRepos
#' @importFrom BiocInstaller biocVersion
#' @importFrom stringr str_split
#' 
#' @export
getBiocPkgList = function(version = biocVersion()) {
  viewsFileUrl = paste(biocinstallRepos(version=version)['BioCsoft'], 'VIEWS', sep = '/')
  ret = as.data.frame(read.dcf(url(viewsFileUrl)))
  # convert comma-delimted text columns into
  # list columns
  commaCols = c('Depends', 'Suggests', 'dependsOnMe', 'Imports', 'importsMe',
                'Enhances', 'vignettes', 'vignetteTitles', 'suggestsMe',
                'Author', 'Maintainer')
  for(commaCol in commaCols) {
    ret[[commaCol]] = str_split(ret[[commaCol]],'\\s?,\\s?')
  }
  ret
}
