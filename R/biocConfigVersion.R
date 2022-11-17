.BIOC_CONFIG_YAML <- "https://bioconductor.org/config.yaml"

#' Create a Bioconductor configuration that allows reproducibility
#'
#' This function helps users who wish to use a previously released Bioconductor
#' version.
#'
#' @details The `version` argument is largely dictated by the current R /
#'   Bioconductor installation, e.g., Bioconductor version 3.15. With this
#'   information and the value of `getOption("BiocManager.snapshot")`, the
#'   function will download a copy of the `config.yaml` file and modify it with
#'   the appropriate date entry as returned from `biocLastBuildDate` for the
#'   Bioconductor version in question. The function will also update the
#'   `.Renviron` file with a `BIOCONDUCTOR_CONFIG_FILE` entry. See
#'   `?BiocManager::install` for more details. This step can be skipped by
#'   setting the `Renviron` argument to `NULL` if the `BIOCONDUCTOR_CONFIG_FILE`
#'   option is set.
#'
#' @param version character(1) The desired version to reproduce. This is largely
#'   dictated by the current R / Bioconductor version installed.
#'
#' @param lastBuildDate named character(1) Override argument to specify the date
#'   of the desired CRAN snapshot, this date should agree or come close to the
#'   date provided by `biocLastBuildDate`. The name of the character vector
#'   should correspond to the Bioconductor version, e.g.,
#'   `c('3.15'="2022-10-19")`. When `lastBuildDate` is not given, the `version`
#'   argument is used in conjunction with the aforementioned function.
#'
#' @param snapshot character(1) The snapshot CRAN repository to use for
#'   reproducibility. This defaults to the value of
#'   `getOption("BiocManager.snapshot", "RSPM")`.
#'
#' @param yamlDir character(1) The folder location for the modified
#'   `config.yaml` file. By default, it is set to the value of
#'   `tools::R_user_dir("BiocPkgTools", which = "config")`.
#'
#' @param Renviron character(1) The file location of the `.Renviron` file to
#'   update with the `BIOCONDUCTOR_CONFIG_FILE` variable. Set to `NULL` to avoid
#'   writing in the `.Renviron`. The default directory location of the file is
#'   given by `Sys.getenv("HOME")`.
#'
#' @return Mostly called for the side-effects of copying and modifying the
#'   `config.yaml` and `.Renviron` files to reproduce an R / Bioconductor
#'   package environment from a previous Bioconductor release.
#'
#' @examples
#' if (interactive()) {
#'   biocConfigVersion("3.15", "RSPM", overwrite = TRUE)
#' }
#' @export
biocConfigVersion <- function(
    version = "3.15",
    lastBuildDate = NULL,
    snapshot = getOption("BiocManager.snapshot", "RSPM"),
    yamlDir = tools::R_user_dir("BiocPkgTools", which = "config"),
    Renviron = file.path(Sys.getenv("HOME"), ".Renviron")
) {
    if (is.null(lastBuildDate))
        lastBuildDate <- biocLastBuildDate(version = version)
    if (!dir.exists(yamlDir))
        dir.create(yamlDir)
    ## download and edit config.yaml file
    yaml_file <- file.path(yamlDir, "config.yaml")
    download.file(.BIOC_CONFIG_YAML, yaml_file)
    config <- readLines(yaml_file)
    if (!snapshot %in% c("RSPM", "MRAN"))
        stop("Unknown 'snapshot' option; use either 'RSPM' or 'MRAN'")
    mapname <- paste0(tolower(snapshot), "_ver_for_bioc_ver")
    mapelement <- c(
        paste0(mapname, ":"),
        paste0("  ", dQuote(names(lastBuildDate)), ": ", dQuote(lastBuildDate))
    )
    indx <- which(startsWith(config, "release_dates")) - 1L
    config <- append(config, mapelement, after = indx)
    message("Updating 'config.yaml' at ", dirname(yaml_file))
    writeLines(config, yaml_file)

    ## update Renviron with config.yaml location
    if (!is.null(Renviron)) {
        message("Updating ", Renviron, " ...")
        .update_renviron(file = Renviron, yaml_file = yaml_file)
    } else {
        message(
            "Set the 'BIOCONDUCTOR_CONFIG_FILE' envvar or option to:\n  ",
            yaml_file,
            "\nSee ?BiocManager::install for details"
        )
    }
}

.update_renviron <- function(file, yaml_file) {
    if (!file.exists(file))
        file.create(file)
    lines <- readLines(file)
    hasconfig <- any(grepl("BIOCONDUCTOR_CONFIG_FILE", lines))
    if (hasconfig)
        stop(
            "'BIOCONDUCTOR_CONFIG_FILE' already exists; ",
            "see ?BiocManager::install"
        )
    lines <- c(lines, paste0(
        "BIOCONDUCTOR_CONFIG_FILE=",normalizePath(yaml_file, winslash = "/")
    ))
    writeLines(lines, file)
}
