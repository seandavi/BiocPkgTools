#' Look up a package's Bioconductor dependencies
#'
#' The function uses the `pkgType` argument to restrict the look up to only the
#' relevant Bioconductor repository. It works for multiple packages of the same
#' type.
#'
#' @inheritParams tools::package_dependencies
#' @inheritParams BiocManager::repositories
#'
#' @param pkg `character(1)` The package for which to look up dependencies.
#'
#' @param pkgType `character()` Any of 'software', 'data-experiment',
#'   'workflows', and / or 'data-annotation' (defaults to all)
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
    recursive = FALSE,
    version = BiocManager::version()
) {
    pkgType <- match.arg(pkgType, several.ok = TRUE)
    repo.name <- .matchGetShortName(pkgType, "biocmanager.names")
    all_db <- utils::available.packages(repos = BiocManager::repositories())
    repo <- BiocManager:::.repositories_bioc(version)[repo.name]
    biocdb <- utils::available.packages(repos = repo)
    res <- tools::package_dependencies(
        pkg, db = all_db, which = which, recursive = recursive
    )
    if (only.bioc)
        lapply(res, function(pkglist) pkglist[pkglist %in% rownames(biocdb)])
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
    version = BiocManager::version(),
    recursive = FALSE
) {
    whiches <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    names(whiches) <- whiches

    stopifnot(is.character(pkg), !is.na(pkg), nzchar(pkg))

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

    pkgType <- match.arg(pkgType, several.ok = TRUE)
    repo.name <- .matchGetShortName(pkgType, "biocmanager.names")
    all_db <- utils::available.packages(repos = BiocManager::repositories())
    repo <- BiocManager:::.repositories_bioc(version)[repo.name]
    biocdb <- utils::available.packages(repos = repo)

    if (recursive)
        res <- tools::package_dependencies(
            pkg, all_db, reverse = TRUE, which = which, recursive = recursive
        )[[pkg]]
    else
        res <- lapply(which, function(ofwhich) {
            tools::package_dependencies(
                pkg, all_db, reverse = TRUE, which = ofwhich, recursive = recursive
            )[[pkg]]
        })


    if (only.bioc && recursive)
        res <- list(recursive = res[res %in% rownames(biocdb)])
    else if (only.bioc)
        res <- lapply(
            res, function(pkglist) pkglist[pkglist %in% rownames(biocdb)]
        )

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
        totals, Total = sum(unlist(totals)), row.names = attr(object, "package")
    )
}
