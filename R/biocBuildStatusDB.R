#' Download and parse the build status information for Bioconductor packages
#'
#' @description This function downloads and parses the build status information
#'  for Bioconductor packages. The build status information is available for
#'  the current release and the previous release. Other versions may be
#'  available.
#'
#' @inheritParams biocBuildReport
#'
#' @return A `data.frame` with the following columns:
#'  * pkg: The name of the package
#'  * node: The builder on which the package was built
#'  * stage: The stage of the build, e.g., 'install', 'buildsrc', 'checksrc',
#'    etc.
#'  * result: The status of the build, e.g., 'OK', 'ERROR', 'WARNINGS', etc.
#'
#' @export
biocBuildStatusDB <- function(
    version = BiocManager::version(),
    pkgType = c(
        "software", "data-experiment", "data-annotation", "workflows"
    )
) {
    if (version %in% c("release", "devel"))
        version <- BiocManager:::.version_bioc(version)

    if (!is.package_version(version))
        stop(
            "'version' is not 'release', 'devel', or a valid 'package_version'"
        )

    pkgType <- match.arg(pkgType, several.ok = TRUE)

    if (version < package_version("3.14"))
        pkgType <- "software"

    pkgType <- .matchGetShortName(pkgType, "stat.url")

    urls <- get_build_status_db_url(version, pkgType)
    names(urls) <- pkgType
    urls <- Filter(.url_exists, urls)

    url_list <- lapply(
        urls,
        function(url) {
            file <- .cache_url_file(url)
            dat <- readLines(file)
            sdat <- strsplit(dat, "#|:\\s")
        }
    )
    sdat <- do.call(
        rbind, unlist(url_list, recursive = FALSE)
    )
    sdat <- as.data.frame(sdat)
    names(sdat) <- c("pkg", "node", "stage", "result")
    attr(sdat, "BioCversion") <- version
    attr(sdat, "retrieved") <- Sys.time()
    sdat
}
