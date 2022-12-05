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
#' @details
#'
#' CRAN packages for out-of-date _Bioconductor_ installations can be
#' installed from historical 'snapshots' consistent with the last date
#' the Bioconductor version was current. This behavior is largely dictated by
#' the actual R/Bioconductor installation, e.g., Bioconductor 3.11. For example,
#' _Bioconductor_ version 3.11 was current until October 28, 2020; CRAN packages
#' are therefore installed from a snapshot created on 2020-10-28. By default,
#' the snapshots are from 'RSPM', the [RStudio Package Manager][RSPM]. Use
#' `options(BiocPkgTools.snapshot = "MRAN")` to instead use the [Microsoft R
#' Archive Network][MRAN], or `options(BiocPkgTools.snapshot = "CRAN")` to use
#' the current CRAN repository (i.e., disabling the snapshot feature).
#'
#' [MRAN]: https://mran.microsoft.com/timemachine
#' [RSPM]: https://packagemanager.rstudio.com/client/#/repos/2/overview
#'
#' Note that the function will temporarily change the `getOption('repos')`
#' setting for `CRAN` to allow installation of CRAN packages from either the
#' [RSPM] or [MRAN] time machines. The function will also modify the
#' `BIOCONDUCTOR_USE_CONTAINER_REPOSITORY` environment variable to temporarily
#' disable binary package installations.
#'
#' It may be desirable to specify different default repositories, especially
#' CRAN, for intentionally out-of-date _Bioconductor_ releases (e.g., to support
#' reproducible research). Use the approach provided by base _R_ to specify
#' alternative repositories, e.g., `options(repos = c(CRAN =
#' "https://mran.microsoft.com/snapshot/2020-02-08"))`. This is supported, but
#' generates an error because specification of an inappropriate CRAN repository
#' (one providing packages not consistent with the dates of the _Bioconductor_
#' release) results in use of CRAN packages not consistent with _Bioconductor_
#' best practices.
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
        CRAN = .repositories_cran(cran)
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
