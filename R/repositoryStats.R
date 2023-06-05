#' Bioconductor Binary Repository Statistics
#'
#' @aliases print.repositoryStats
#'
#' @description Summarize binary packages compatible with the Bioconductor or
#'   Terra container in use.
#'
#' @param version (Optional) `character(1)` or `package_version`
#'   indicating the _Bioconductor_ version (e.g., "3.8") for which
#'   repositories are required.
#'
#' @param binary_repository `character(1)` location of binary repository as
#'   given by `BiocManager::containerRepository` (default)
#'
#' @param ... further arguments passed to or from other methods (not used).
#'
#' @return a list of class `repositoryStats` with the following fields:
#' \itemize{
#'
#' \item{container: }{character(1) container label, e.g.,
#' \code{bioconductor_docker}, or NA if not evaluated on a supported
#' container}
#'
#' \item{bioconductor_version: }{\code{package_version} the
#' Bioconductor version provided by the user.}
#'
#' \item{repository_exists: }{logical(1) TRUE if a binary repository
#' exists for the container and Bioconductor_Version version.}
#'
#' \item{bioconductor_binary_repository: }{character(1) repository
#' location, if available, or NA if the repository does not exist.}
#'
#' \item{n_software_packages: }{integer(1) number of software packages
#' in the Bioconductor source repository.}
#'
#' \item{n_binary_packages: }{integer(1) number of binary packages
#' available. When a binary repository exists, this number is likely
#' to be larger than the number of source software packages, because
#' it includes the binary version of the source software packages, as
#' well as the (possibly CRAN) dependencies of the binary packages}
#'
#' \item{n_binary_software_packages: }{integer(1) number of binary
#' packages derived from Bioconductor source packages. This number is
#' less than or equal to \code{n_software_packages}.}
#'
#' \item{missing_binaries: }{integer(1) the number of Bioconductor
#' source software packages that are not present in the binary
#' repository.}
#'
#' \item{out_of_date_binaries: }{integer(1) the number of Bioconductor
#' source software packages that are newer than their binary
#' counterpart. A newer source software package
#' might occur when the main Bioconductor build system has
#' updated a package after the most recent run of the binary
#' build system.}
#' }
#'
#' @importFrom utils available.packages
#' @importFrom httr HEAD headers
#' @importFrom BiocManager containerRepository
#'
#' @examples
#' stats <- repositoryStats() # obtain statistics
#' stats                       # display a summary
#' stats$container             # access an element for further computation
#'
#' @export
repositoryStats <- function(
    version = BiocManager::version(),
    binary_repository = BiocManager::containerRepository(version)
) {
    platform_docker <- BiocManager:::.repository_container_version()
    container <- platform_docker$platform
    bioc_repository <- suppressMessages({
        BiocManager::repositories()[["BioCsoft"]]
    })
    db_bioc <- available.packages(repos = bioc_repository)
    if (length(binary_repository)) {
        db_binary <- available.packages(repos = binary_repository)
        packages <- paste0(contrib.url(binary_repository), "/PACKAGES")
        response <- HEAD(packages)
        last_modified <- headers(response)$`last-modified`
        PACKAGES_mtime <-
            format(strptime(
                last_modified, "%a, %d %b %Y %H:%M", tz = "UTC"
            ), usetz = TRUE)
    } else {
        db_binary <- db_bioc[NULL,]
        PACKAGES_mtime <- NA_character_
    }

    missing_binaries <- setdiff(rownames(db_bioc), rownames(db_binary))
    found_binaries <- intersect(rownames(db_bioc), rownames(db_binary))

    bioc_versions <- package_version(db_bioc[found_binaries, "Version"])
    binary_versions <- package_version(db_binary[found_binaries, "Version"])
    binary_out_of_date <- bioc_versions > binary_versions
    n_out_of_date_binaries <- sum(binary_out_of_date)
    out_of_date_binaries <- found_binaries[binary_out_of_date]

    query_timestamp = format(
        Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC", usetz = TRUE
    )

    result <- list(
        container = if (nzchar(container)) container else NA_character_,
        bioconductor_version = version,
        bioconductor_binary_repository =
            if (length(binary_repository)) binary_repository else NA_character_,
        PACKAGES_mtime = PACKAGES_mtime,
        query_timestamp = query_timestamp,
        repository_exists = length(binary_repository) > 0L,
        n_software_packages = nrow(db_bioc),
        n_binary_packages = nrow(db_binary),
        n_binary_software_packages = length(found_binaries),
        missing_binaries = missing_binaries,
        out_of_date_binaries = out_of_date_binaries
    )
    class(result) <- c("repositoryStats", class(result))
    result
}

.repositoryStats_package_format <-
    function(x)
{
    msg <- paste(sort(x), collapse = " ")
    paste(strwrap(msg, indent = 2L, exdent = 2L), collapse = "\n")
}

#' @describeIn repositoryStats Print a summary of package
#'     availability in binary repositories.
#'
#' @param x the object returned by `repositoryStats()`.
#'
#' @export
print.repositoryStats <-
    function(x, ...)
{
    bioconductor_binary_repository <- ifelse(
        is.na(x$bioconductor_binary_repository),
        paste(" ", x$bioconductor_binary_repository),
        paste("\n  ", x$bioconductor_binary_repository)
    )
    cat(
        "Container: ", x$container, "\n",
        "Bioconductor version: ", as.character(x$bioconductor_version), "\n",
        "Bioconductor binary repos:", bioconductor_binary_repository, "\n",
        "PACKAGES timestamp: ", x$PACKAGES_mtime, "\n",
        "Query timestamp: ", x$query_timestamp, "\n",
        "Bioconductor software packages: ", x$n_software_packages, "\n",
        "Binary packages: ", x$n_binary_packages, "\n",
        "Binary software packages: ", x$n_binary_software_packages, "\n",
        "Missing binary software packages: ", length(x$missing_binaries), "\n",
        if (x$repository_exists)
            .repositoryStats_package_format(x$missing_binaries),
        "Out-of-date binary software packages: ",
            length(x$out_of_date_binaries), "\n",
        if (x$repository_exists)
            .repositoryStats_package_format(x$out_of_date_binaries),
        sep = ""
    )
}

