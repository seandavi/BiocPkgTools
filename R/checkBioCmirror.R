#' Check whether a Bioconductor mirror is configured
#'
#' The function checks the `PACKAGES` file with `RCurl::url.exists`.
#'
#' @param mirror character(1) The Bioconductor mirror to be tested resolves from
#'   `getOption("BioC_mirror")`. If one is not selected, `chooseBioCmirror()`
#'   will be run and the user will be able to select a mirror interactively.
#'
#' @param version character(1) The Bioconductor version to be checked.
#'
#' @param repoType character(1) One of the Bioconductor package types, including
#'   `BioCsoft` (default), `BioCann`, `BioCexp`, `BioCworkflows`, and
#'   `BioCbooks`.
#'
#' @param type character(1) The package type whether "source", "binary",
#'   "mac.binary", or "win.binary" as given by `getOption("pkgType")`. See
#'   `install.packages` for details.
#'
#' @export
checkBioCmirror <- function(
    mirror = getOption("BioC_mirror"),
    version = BiocManager::version(),
    repoType =
        c("BioCsoft", "BioCann", "BioCexp", "BioCworkflows", "BioCbooks"),
    type = getOption("pkgType")
) {
    if (is.null(mirror)) {
        chooseBioCmirror()
        mirror <- getOption("BioC_mirror")
    }
    repo_type <- match.arg(repoType)
    path <- repo_short_names[
        repo_short_names[["repo.name"]] == repoType, "json.file"
    ]
    bioc_repo <-
        paste(mirror, "packages", as.character(version), path, sep = "/")
    url <- paste0(contrib.url(bioc_repo, type = type), "/PACKAGES")
    url_exists <- RCurl::url.exists(url)
    names(url_exists) <- url
    url_exists
}
