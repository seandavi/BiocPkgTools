.repositories_rspm <- function(cran, rspm_version) {
    if (is.na(rspm_version)) {
        cran
    } else {
        rspm_version <- as.Date(rspm_version, "%Y-%m-%d")
        if (is.na(rspm_version))
            stop("'RSPM' date format does not match '%Y-%m-%d'")
        paste0("https://packagemanager.rstudio.com/cran/", rspm_version)
    }
}

.repositories_mran <- function(cran, mran_version) {
    if (is.na(mran_version)) {
        cran
    } else {
        mran_version <- as.Date(mran_version, "%Y-%m-%d")
        if (is.na(mran_version))
            stop("'MRAN' date format does not match '%Y-%m-%d'")
        paste0("https://mran.microsoft.com/snapshot/", mran_version)
    }
}

.repositories_cran <- function(cran) {
    if (identical(cran, c(CRAN = "@CRAN@")) || is.na(cran))
        "https://cloud.r-project.org"
    else
        cran
}

#' Install packages from a previous release of Bioconductor for reproducibility
#'
#' This function allows users to install packages from a previously released
#' Bioconductor version.
#'
#' @details The `version` argument is largely dictated by the actual R /
#'   Bioconductor installation, e.g., Bioconductor version 3.14. With this
#'   information and the value of `getOption("BiocPkgTools.snapshot")`, the
#'   function will temporarily change the `getOption('repos')` setting for
#'   `CRAN` to allow installation of CRAN packages from either the RSPM or MRAN
#'   time machines. The date of the 'snapshot' coincides with the _last_ release
#'   date for the indicated Bioconductor version. The function will also modify
#'   the `BIOCONDUCTOR_USE_CONTAINER_REPOSITORY` environment variable to
#'   temporarily disable binary package installations.
#'
#' @inheritParams BiocManager::install
#'
#' @param version character(1) The desired version to reproduce. This is largely
#'   dictated by the current R / Bioconductor version installed and is indicated
#'   by `BiocManager::version` by default.
#'
#' @param snapshot character(1) The snapshot CRAN repository to use for
#'   reproducibility. This defaults to the value of
#'   `getOption("BiocPkgTools.snapshot", "RSPM")`.
#'
#' @return Mostly called for the side-effects of copying and modifying the
#'   `config.yaml` and `.Renviron` files to reproduce an R / Bioconductor
#'   package environment from a previous Bioconductor release.
#'
#' @examples
#'
#' if (interactive()) {
#'   archiveInstall("DESeq2", version = "3.14")
#' }
#'
#' @export
archiveInstall <- function(
    pkgs = character(),
    version = BiocManager::version(),
    snapshot = getOption("BiocPkgTools.snapshot", "RSPM"),
    ...
) {
    repos <- getOption("repos")
    last_date <- biocLastBuildDate(version = version)
    if (is.na(last_date))
        stop("The 'version' ", version, " archive is not supported")
    valid <- c("CRAN", "MRAN", "RSPM")
    if (length(snapshot) != 1L || !snapshot %in% valid)
        .stop(
            "'getOption(\"BiocPkgTools.snapshot\")' must be one of %s",
            paste0("'", valid, "'", collapse = " ")
        )
    cran <- repos["CRAN"]
    rename <- repos == "@CRAN@" | names(repos) == "CRAN"
    repos[rename] <- switch(
        snapshot,
        RSPM = .repositories_rspm(cran, last_date),
        MRAN = .repositories_mran(cran, last_date),
        CRAN = .repositories_cran(cran),
        stop("unknown option 'BiocPkgTools.snapshot = \"%s\"'", snapshot)
    )
    old_opt <- options(repos = repos["CRAN"])
    on.exit(options(old_opt))
    use_binaries <- Sys.getenv(
        "BIOCONDUCTOR_USE_CONTAINER_REPOSITORY", names = TRUE, unset = FALSE
    )
    if (!identical(use_binaries, "FALSE")) {
        Sys.setenv(
            BIOCONDUCTOR_USE_CONTAINER_REPOSITORY = FALSE
        )
        on.exit(do.call(Sys.setenv, as.list(use_binaries)))
    }
    BiocManager::install(pkgs = pkgs, ...)
}
