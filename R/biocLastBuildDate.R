#' Obtain the last build date for a particular Bioconductor version
#'
#' The function facilitates the discovery of last build dates useful for
#' selecting a fixed date to be used in conjunction with
#' `options(BiocPkgTools.snapshot)`. Currently, it looks at
#' <https://bioconductor.org/checkResults/> and parses the dates listed.
#'
#' @param version character(1) Indicates the Bioconductor version for which the
#'   last build date is sought. By default, 'all' versions will be returned.
#'
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#'
#' @examples
#'
#' biocLastBuildDate(version = "3.14")
#'
#' @export
biocLastBuildDate <- function(version = "all") {
    stopifnot(
        identical(length(version), 1L), !is.na(version), is.character(version)
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
    if (identical(version, "all"))
        version <- names(last_bioc_dates)
    else
        version <- as.character(version)
    last_bioc_dates[version]
}
