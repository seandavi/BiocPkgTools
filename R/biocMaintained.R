#' Bioconductor Maintained Packages
#'
#' List all the packages associated with a maintainer. By default, it will
#' return all packages associated with the `maintainer@bioconductor.org` email.
#' `hasBiocMaint` returns a logical vector corresponding to the input character
#' vector of packages indicating whether any package is maintained by the
#' Bioconductor core team.
#'
#' @param main character(1) The regex for searching through the Maintainer
#' column as obtained from `biocPkgList()`.
#'
#' @inheritParams biocBuildReport
#' @inheritParams biocDownloadStats
#'
#' @return For `biocMaintained`: a `tibble` of packages associated with the
#'   maintainer.
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

#' @rdname biocMaintained
#'
#' @param pkg `character(1)` A vector of package names (case sensitive).
#'
#' @param repo `character()` A vector of Bioconductor repositories to search
#'   through. By default, it will search through all Bioconductor repositories.
#'
#' @inheritParams biocPkgList
#'
#' @return For `hasBiocMaint`: a logical vector indicating whether the
#'   package is maintained by Bioconductor.
#'
#' @export
hasBiocMaint <- function(
    pkg,
    version = BiocManager::version(),
    repo = c("BioCsoft", "BioCexp", "BioCworkflows", "BioCann")
) {
    pkgs <- biocPkgList(
        version = version, repo = repo, addBiocViewParents = FALSE
    )
    res <- vapply(
        unlist(pkgs[pkgs$Package %in% pkg, "Maintainer"], recursive = FALSE),
        function(maint) {
            any(grepl("maintainer@bioconductor\\.org", maint))
        },
        logical(1L)
    )
    names(res) <- pkgs$Package
    res
}
