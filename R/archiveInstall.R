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

archiveInstall <-
    function(pkgs = character(), version = BiocManager::version(), ...)
{
    repos <- getOption("repos")
    last_date <- biocLastBuildDate(version = version)
    ## default <- if (version() > "3.11") "MRAN" else "CRAN"
    opt <- getOption("BiocManager.snapshot", "CRAN")
    valid <- c("CRAN", "MRAN", "RSPM")
    if (length(opt) != 1L || !opt %in% valid)
        .stop(
            "'getOption(\"BiocManager.snapshot\")' must be one of %s",
            paste0("'", valid, "'", collapse = " ")
        )
    cran <- repos["CRAN"]
    rename <- repos == "@CRAN@" | names(repos) == "CRAN"
    repos[rename] <- switch(
        opt,
        RSPM = .repositories_rspm(cran, last_date),
        MRAN = .repositories_mran(cran, last_date),
        CRAN = .repositories_cran(cran),
        stop("unknown option 'BiocManager.snapshot = \"%s\"'", opt)
    )
    options(repos = repos["CRAN"])
    BiocManager::install(pkgs = pkgs, ...)
}
