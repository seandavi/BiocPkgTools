#' Bioconductor Maintained Packages
#'
#' List all the packages associated with a maintainer. By default, it will
#' return all packages associated with the `maintainer@bioconductor.org` email.
#'
#' @inheritParams biocBuildReport
#'
#' @param main character(1) The regex for searching through the Maintainer
#' column as obtained from `biocPkgList()`.
#'
#' @md
#'
#' @examples
#'
#' biocMaintained()
#'
#' @export
biocMaintained <-
    function(
        version = BiocManager::version(),
        main = "maintainer@bioconductor\\.org"
    )
{
    pkgs <- BiocPkgTools::biocPkgList()
    biocmaint <- vapply(
        pkgs$Maintainer,
        function(x)
            any(grepl(main, x)),
        logical(1L)
    )
    pkgs[biocmaint, ]
}

