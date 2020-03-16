#' Explore Bioconductor packages interactively
#' @description Explore Bioconductor packages through an interactive bubble plot. Click on bubbles to bring up additional information about the package. Size and proximity to center of a bubble is based on the downloads the package has in the past month.
#' @param top maximum number of packages displayed in any biocView
#' @param ... parameters passed to \code{htmlwidgets::createWidget()}
#'
#' @importFrom htmlwidgets createWidget
#'
#' @return A bubble plot of Bioconductor packages
#'
#' @export
biocExplore <- function(top = 500L, ...) {
    if(!is.numeric(top) || top < 1){
        stop("top must be >= 1")
    }
    # instruction messages
    message("- Hover over bubbles to see full name and lifetime downloads")
    message("- Click on bubbles to see more information about package")
    message("- Use filter to filter by biocViews")

    data <- list(
        data = get_bioc_data(),
        top = top
    )

    settings <- list(
    )

    x <- list(
        data = data,
        settings = settings
    )

    # create the widget
    # create widget
    htmlwidgets::createWidget(
        name = 'bioc_explore',
        package = 'BiocPkgTools',
        x = x,
        ...
    )
}
