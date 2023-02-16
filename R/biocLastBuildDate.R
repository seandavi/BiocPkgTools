#' @name BiocPkgTools-deprecated
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
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#'
#' @examples
#'
#' biocLastBuildDate(version = "3.14")
#'
#' @export
biocLastBuildDate <- function(version) {
    .Deprecated(
        "lastBuilt", "BiocArchive",
        c(
            "'biocLastBuildDate' is deprecated.\n",
            "Use 'BiocArchive::lastBuilt' instead.\n",
            "See help(\"BiocPkgTools-deprecated\")"
        )
    )
    if (!requireNamespace("lubridate", quietly = TRUE))
        stop("Install 'lubridate' to run 'biocLastBuildDate'")
    buildrep <- read_html("https://bioconductor.org/checkResults/")
    nodes <- html_nodes(buildrep, "div")[-1:-2]
    bioc_names <- html_text(html_nodes(nodes, "h3"))
    bioc_vers <- gsub("Bioconductor ", "", bioc_names, fixed = TRUE)
    names(nodes) <- bioc_vers
    softstring <- "Software packages: last results"
    builddates <- grep(
        softstring, html_text(xml_find_all(nodes, ".//li[1]")),
        fixed = TRUE, value = TRUE
    )
    lastdates <- gsub(
        ".*\\(([A-Za-z]{3,5}\\ [0-9]{1,2},\\ [0-9]{4}).*", "\\1", builddates
    )
    last_bioc_dates <- format(lubridate::mdy(lastdates), "%Y-%m-%d")
    names(last_bioc_dates) <- names(lastdates)
    if (missing(version))
        last_bioc_dates
    else
        last_bioc_dates[version]
}
