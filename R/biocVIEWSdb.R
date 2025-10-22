.get_VIEWS_url <- function(version, type) {
    bioc_repos <- BiocManager:::.repositories_bioc(version = version)
    paste0(bioc_repos[type], "/VIEWS")
}

.read_VIEWS_url <- function(url) {
    file <- .cache_url_file(url)
    tryCatch({
        read.dcf(file) |>
            as.data.frame()
    }, error = function(e) {
        stop(
            "Error reading VIEWS file from URL: ",
            url,
            "\n",
            conditionMessage(e)
        )
    })
}

.get_VIEWS <- function(version, type) {
    views_url <- .get_VIEWS_url(version = version, type = type)
    .read_VIEWS_url(views_url)
}

#' Parse and return the VIEWS file for a Bioconductor Release
#'
#' The function parses and returns the `VIEWS` file for a specified Bioconductor
#' version, either "release" or "devel". The `VIEWS` file contains metadata
#' about Bioconductor packages, including information about their categories,
#' topics, and other details.
#'
#' @inheritParams biocBuildReport
#'
#' @examplesIf interactive()
#' biocVIEWSdb(pkgType = "software")
#' @export
biocVIEWSdb <- function(
    version = BiocManager::version(),
    pkgType = c("software", "data-experiment", "data-annotation", "workflows")
) {
    if (version %in% c("release", "devel"))
        version <- BiocManager:::.version_bioc(version)

    if (!is.package_version(version))
        stop(
            "'version' is not 'release', 'devel', or a valid 'package_version'"
        )

    pkgType <- match.arg(pkgType, several.ok = TRUE)
    pkgType <- .matchGetShortName(pkgType, "biocmanager.names")

    views_list <- structure(
        vector("list", length(pkgType)),
        .Names = pkgType
    )
    for (type in pkgType) {
        views_df <- .get_VIEWS(version = version, type = type)
        if (nrow(views_df))
            views_list[[type]] <- views_df
    }
    dplyr::bind_rows(views_list)
}
