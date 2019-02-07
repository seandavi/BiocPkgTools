#' Get full bioc software package listing, with details
#'
#' The BiocViews-generated \code{VIEWS} file is available
#' for bioconductor release and devel repositories. It
#' contains quite a bit more information from the
#' package \code{DESCRIPTION} files than the \code{PACKAGES}
#' file. In particular, it contains \code{biocViews} annotations
#' and URLs for vignettes and developer URLs.
#'
#' @param version The requested bioconductor version. Will
#'     default to use the BiocManager defaults (ie., \code{version()}).
#' @param repo The requested biooconductor repository. The default will be the
#'    Bioconductor software repository: BioCsoft. Available repos include:
#'    "BioCsoft", "BioCann", "BioCexp", "BioCworkflows", and "CRAN". Note
#'    that not all repos are available for all versions, particularly older
#'    versions (but who would use those, right?).
#'
#' @return an object of class \code{tbl_df}.
#'
#' @importFrom BiocManager repositories version
#' @importFrom stringr str_split str_replace_all str_remove_all str_squish
#' @importFrom tibble as_tibble
#'
#' @examples
#' bpkgl = biocPkgList("3.7")
#' bpkgl
#' unlist(bpkgl[1,'Depends'])
#'
#' # Get a list of all packages that
#' # import "GEOquery"
#' library(dplyr)
#' bpkgl %>%
#'   filter(Package=='GEOquery') %>%
#'   pull(c('importsMe'))
#'
#' @export
biocPkgList = function(version = BiocManager::version(), repo='BioCsoft') {
    viewsFileUrl = paste(BiocManager::repositories(version = version)[repo], 'VIEWS', sep = '/')
    con = url(viewsFileUrl)
    ret = as.data.frame(read.dcf(con), stringsAsFactors = FALSE)
    close(con)
    # convert comma-delimted text columns into
    # list columns
    commaCols = c('Depends', 'Suggests', 'dependsOnMe', 'Imports', 'importsMe',
                'Enhances', 'vignettes', 'vignetteTitles', 'suggestsMe',
                'Maintainer', 'biocViews')
    for(commaCol in commaCols) {
        ret[[commaCol]] = str_split(ret[[commaCol]],'\\s?,\\s?')
    }

    ret[["Author"]] = ret[["Author"]] %>%
        str_replace_all("\n", " ") %>%
        str_remove_all("\\[.*?\\]") %>%
        str_remove_all("<.*?>") %>%
        str_remove_all("\\(.*?\\)") %>%
        str_squish() %>%
        str_replace_all("\\w* contributions ?\\w*", ", ") %>%
        str_replace_all("\\sand\\s", ", ") %>%
        str_replace_all(",\\s+,", ",") %>%
        str_replace_all(",+", ",")

    ret[["Author"]] = lapply(
        str_split(ret[["Author"]], ","),
        str_squish
    )

    ret = as_tibble(ret)
    class(ret) = c("biocPkgList", class(ret))
    ret
}

stripVersionString = function(s) sub('\\s?\\(.*\\)\\s?','',s)
