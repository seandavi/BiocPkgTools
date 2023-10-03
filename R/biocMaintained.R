#' Bioconductor Maintained Packages
#'
#' List all the packages associated with a maintainer. By default, it will
#' return all packages associated with the `maintainer@bioconductor.org` email.
#'
#' @param main character(1) The regex for searching through the Maintainer
#' column as obtained from `biocPkgList()`.
#'
#' @inheritParams biocBuildReport
#' @inheritParams biocDownloadStats
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
        main = "maintainer@bioconductor\\.org",
        version = BiocManager::version(),
        pkgType = c(
            "software", "data-experiment",
            "workflows", "data-annotation"
        )
    )
{
    pkgType <- match.arg(pkgType, several.ok = TRUE)
    repos <- .matchGetShortName(pkgType, "repo.name")
    pkgs <- biocPkgList(version = version, repo = repos)
    biocmaint <- vapply(
        pkgs$Maintainer,
        function(x)
            any(grepl(main, x)),
        logical(1L)
    )
    pkgs[biocmaint, ]
}

