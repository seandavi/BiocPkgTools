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

#' Obtain all the reverse dependencies for a Bioconductor package
#'
#' The function returns a slightly upgraded list with dependency types as
#' elements and package names in each of those elements, if any. The
#' types of dependencies can be seen in the `which` argument documentation.
#'
#' @details The summary method of the `biocrevdeps` class given by
#'   `pkgBiocRevDeps` provides a tally in each dependency field.
#'
#' @inheritParams pkgBiocDeps
#'
#' @return A `biocrevdeps` list class object
#'
#' @examples
#' rdeps <- pkgBiocRevDeps("MultiAssayExperiment", which = "all")
#' rdeps
#' summary(rdeps)
#' @export
pkgBiocRevDeps <- function(
    pkg,
    pkgType = c(
        "software", "data-experiment", "workflows", "data-annotation"
    ),
    which = "all",
    only.bioc = TRUE,
    version = BiocManager::version()
) {
    whiches <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    names(whiches) <- whiches

    if (!all(which %in% c(whiches, "all", "strong", "most")))
        stop("'which' dependency type is invalid; see ?package_dependencies")

    lwhich <- switch(
        which,
        all = TRUE,
        strong = c(TRUE, TRUE, TRUE, FALSE, FALSE),
        most = c(TRUE, TRUE, TRUE, TRUE, FALSE),
        whiches %in% which
    )
    which <- whiches[lwhich]

    pkgType <- match.arg(pkgType)
    repo.name <- .matchGetShortName(pkgType, "repo.name")
    repo <- BiocManager:::.repositories_bioc(version)[repo.name]
    db <- utils::available.packages(
        repos = BiocManager:::.repositories_bioc(version)[repo.name]
    )

    res <- lapply(which, function(ofwhich) {
        tools::package_dependencies(
            pkg, db, reverse = TRUE, which = ofwhich
        )[[pkg]]
    })

    if (only.bioc)
        res <- lapply(res, function(pkglist) pkglist[pkglist %in% rownames(db)])

    attributes(res) <-
        list(package = pkg, class = "biocrevdeps", names = names(res))
    res
}

#' @rdname pkgBiocRevDeps
#'
#' @inheritParams base::summary
#'
#' @export
summary.biocrevdeps <- function(object, ...) {
    totals <- lapply(object, length)
    data.frame(
        totals, Total = sum(unlist(totals)), row.names = attr(obj, "package")
    )
}
