#' Parse the Build Report tarball for a Bioconductor release
#'
#' @description This function parses the Build Report tarball for a Bioconductor
#'   release. By default it will pull all the `report.tgz` files for each
#'   Bioconductor package type. The Bioconductor Build System (BBS) Build Report
#'   tarball contains build status information for all packages in a
#'   Bioconductor release. This function is mainly used by [biocBuildReport()].
#'
#' @inheritParams biocBuildStatusDB
#' @inheritParams biocBuildReport
#'
#' @export
biocBuildReportDB <- function(
    version = BiocManager::version(),
    pkgType = c("software", "data-experiment", "data-annotation", "workflows"),
    stage.timings = FALSE
) {
    if (version %in% c("release", "devel"))
        version <- BiocManager:::.version_bioc(version)

    pkgType <- match.arg(pkgType, several.ok = TRUE)
    pkgType <- .matchGetShortName(pkgType, "stat.url")

    urls <- .get_build_report_tgz_url(version, pkgType)
    names(urls) <- pkgType
    urls <- Filter(.url_exists, urls)

    tempfolders <- replicate(length(urls), tempfile())

    dcf_tables <- Map(
        function(url, tempfolder) {
            treport <- .cache_url_file(url)
            untar(treport, exdir = tempfolder)
            .read_info_dcfs(tempfolder)
        }, url = urls, tempfolder = tempfolders
    )
    report_table <- do.call(rbind.data.frame, unname(dcf_tables))
    report_table[["pkgType"]] <- rep(names(urls), vapply(dcf_tables, nrow, 1L))
    attr(report_table, "dcf_folder") <- tempfolders
    report_table
}

