#' Get full biocViews file
#'
#' The BiocViews-generated \code{VIEWS} file is available
#' for bioconductor release and devel repositories. It
#' contains quite a bit more information from the
#' package \code{DESCRIPTION} files than the \code{PACKAGES}
#' file. In particular, it contains \code{biocViews} annotations
#' and URLs for vignettes and developer URLs.
#'
#' @param biocVersion The requested bioconductor version. Will
#'     default to use the BiocInstaller defaults (ie., \code{biocVersion()}).
#'
#' @return a \code{data.frame}
#'
#' @importFrom BiocInstaller biocinstallRepos
#' @importFrom BiocInstaller biocVersion
#'
#' @export
getBiocViewsFile = function(version = biocVersion()) {
  viewsFileUrl = paste(biocinstallRepos(version=version)['BioCsoft'], 'VIEWS', sep = '/')
  as.data.frame(read.dcf(url(viewsFileUrl)))
}
