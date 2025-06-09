#' Bioconductor Maintained Packages
#'
#' List all the packages associated with a maintainer. By default, it will
#' return all packages associated with the `maintainer@bioconductor.org` email.
#' `hasBiocMaint` returns a logical vector corresponding to the input character
#' vector of packages indicating whether any package is maintained by the
#' Bioconductor core team.
#'
#' @param main `character(1)` A regex string to search for in the `Maintainer`
#'   column from the `biocPkgList()` output.
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
        main = "maintainer@bioconductor.org",
        version = BiocManager::version(),
        pkgType = c(
            "software", "data-experiment",
            "workflows", "data-annotation"
        )
    )
{
    pkgType <- match.arg(pkgType, several.ok = TRUE)
    repos <- .matchGetShortName(pkgType, "biocmanager.names")
    pkgs <- biocPkgList(version = version, repo = repos)
    biocmaint <- vapply(
        pkgs$Maintainer,
        function(x)
            any(grepl(main, x, ignore.case = TRUE)),
        logical(1L)
    )
    filtered <- pkgs[biocmaint, ]
    attr(filtered, "version") <- version
    attr(filtered, "maintainer") <- main
    attr(filtered, "pkgType") <- pkgType
    filtered
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
#' @examples
#' ## maintained by Hervé and not maintainer at bioconductor dot org
#' hasBiocMaint("BiocGenerics")
#'
#' @export
hasBiocMaint <- function(
    pkg,
    version = BiocManager::version(),
    main = "maintainer@bioconductor\\.org",
    repo = c("BioCsoft", "BioCexp", "BioCworkflows", "BioCann")
) {
    pkgs <- biocPkgList(
        version = version, repo = repo, addBiocViewParents = FALSE
    )
    match_idx <- match(pkg, pkgs[["Package"]])
    pkgs <- pkgs[match_idx, c("Package", "Maintainer")]
    missing_pkgs <- is.na(pkgs[["Package"]])
    if (any(missing_pkgs))
        pkgs[missing_pkgs, "Package"] <- pkg[missing_pkgs]

    res <- vapply(
        pkgs[["Maintainer"]],
        function(maint) {
            any(grepl(main, maint))
        },
        logical(1L)
    )
    names(res) <- pkgs[["Package"]]
    res
}
