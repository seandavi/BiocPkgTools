#' @name BiocPkgTools-defunct
#'
#' @aliases biocLastBuildDate
#'
#' @title Deprecated functions in `BiocPkgTools`
#'
#' @description Currently, `biocLastBuildDate` is **DEPRECATED**. See
#'   functionality in `BiocArchive`.
#'
#' @details The function facilitates the discovery of last build dates useful
#'   for selecting a fixed date. Currently, it looks at
#'   <https://bioconductor.org/checkResults/> and parses the dates listed.
#'
#'
#' @seealso <https://github.com/LiNk-NY/BiocArchive>
#'
#' @param version character(1) Indicates the Bioconductor version for which the
#'   last build date is sought.
#'
#' @export
biocLastBuildDate <- function(version) {
    .Defunct(
        "lastBuilt", "BiocArchive",
        c(
            "'biocLastBuildDate' is deprecated.\n",
            "Use 'BiocArchive::lastBuilt' instead.\n",
            "See help(\"BiocPkgTools-defunct\")"
        )
    )
}
