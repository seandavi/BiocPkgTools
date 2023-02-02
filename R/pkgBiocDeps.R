#' Look up a packages Bioconductor dependencies
#'
#' The function uses the `pkgType` argument to restrict the look up to only the
#' relevant Bioconductor repository. It works for multiple packages of the same
#' type.
#'
#' @inheritParams tools::package_dependencies
#' @inheritParams BiocManager::repositories
#' @inheritParams pkgDownloadStats
#'
#' @param only.bioc `logical(1)` Whether to only return Bioconductor
#'   dependencies in the list (default `TRUE`)
#'
#' @examples
#'
#' pkgBiocDeps("MultiAssayExperiment", only.bioc = TRUE)
#'
#' pkgBiocDeps("MultiAssayExperiment", only.bioc = FALSE)
#'
#' @export
pkgBiocDeps <- function(
    pkg,
    pkgType = c(
        "software", "data-experiment", "workflows", "data-annotation"
    ),
    which = "strong",
    only.bioc = TRUE,
    version = BiocManager::version()
) {
    pkgType <- match.arg(pkgType)
    repo.name <- .matchGetShortName(pkgType, "repo.name")
    repo <- BiocManager:::.repositories_bioc(version)[repo.name]
    db <- utils::available.packages(repos = repo)
    res <- tools::package_dependencies(pkg, db = db, which = which)
    if (only.bioc)
        lapply(res, function(pkglist) pkglist[pkglist %in% rownames(db)])
    else
        res
}
