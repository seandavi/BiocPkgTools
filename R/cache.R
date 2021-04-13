.get_cache <- function() {
    cache <- getOption("pkgToolsCache", setCache(verbose = FALSE))
    BiocFileCache::BiocFileCache(cache)
}

.getAnswer <- function(msg, allowed)
{
    if (interactive()) {
        repeat {
            cat(msg)
            answer <- readLines(n = 1)
            if (answer %in% allowed)
                break
        }
        tolower(answer)
    } else {
        "n"
    }
}

#' @name BiocPkgTools-cache
#'
#' @title Manage cache for BiocPkgTools
#'
#' @description Managing user data is important to allow use of email functions
#' such as `biocBuildEmail` and made easy with `BiocFileCache`.
#'
#' @section pkgToolsCache:
#' Get the directory location of the cache. It will prompt the user to create
#' a cache if not already created. A specific directory can be used via
#' \code{setCache}.
#'
#' @section setCache:
#' Specify the directory location of the data cache. By default, it will
#' got to the user's home/.cache/R and "appname" directory as specified by
#' \code{tools::R_user_dir} (with package="BiocPkgTools" and which="cache").
#'
#' @param directory The file location where the cache is located. Once set
#' future downloads will go to this folder.
#' @param verbose Whether to print descriptive messages
#' @param ask logical (default TRUE when interactive session) Confirm the file
#' location of the cache directory
#'
#' @param ... For \code{pkgToolsCache}, arguments are passed to \code{setCache}
#'
#' @importFrom rappdirs user_cache_dir
#'
#' @export
setCache <-
    function(directory = tools::R_user_dir("BiocPkgTools", "cache"),
        verbose = TRUE,
        ask = interactive())
{
    stopifnot(is.character(directory),
        length(directory) == 1L, !is.na(directory))

    if (!dir.exists(directory)) {
        if (ask) {
            qtxt <- sprintf(
                "Create BiocPkgTools cache at \n    %s? [y/n]: ",
                directory
            )
            answer <- .getAnswer(qtxt, allowed = c("y", "Y", "n", "N"))
            if ("n" == answer)
                stop("'BiocPkgTools' directory not created. Use 'setCache'")
        }
        dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    }
    options("pkgToolsCache" = directory)

    if (verbose)
        message("BiocPkgTools cache directory set to:\n    ",
            directory)
    invisible(directory)
}

#' @rdname BiocPkgTools-cache
#' @export
pkgToolsCache <- function(...) {
    getOption("pkgToolsCache", setCache(..., verbose = FALSE))
}
