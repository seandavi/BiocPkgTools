#' Generate needed information to create DOI from a package
#' directory.
#'
#' @param dir character(1) Path to package
#'
#' @import utils
#' @import tools
#'
#' @return A \code{data.frame}
#'
#' @keywords Internal
#'
#' @export
getPackageInfo <- function(dir){
    if(!dir.exists(file.path(dir)))
        stop('dir "',dir,'" does not exist.')
    if(!file.exists(file.path(dir, "DESCRIPTION")))
        stop('dir "',dir,'" does not contain a DESCRIPTION file.')
    # the next line avoids warnings about unexported
    # functions. see: https://github.com/rstudio/learnr/commit/8cf2ad60993108079f38123300b23d96ef58bd4c
    db <- asNamespace('tools')$.read_description(file.path(dir, "DESCRIPTION"))
    Package <- unname(db["Package"])
    aar <- db["Authors@R"]
    if (is.na(aar)) {
        Author <- unname(db["Author"])
    } else {
        # the next line avoids warnings about unexported
        # functions. see: https://github.com/rstudio/learnr/commit/8cf2ad60993108079f38123300b23d96ef58bd4c
        aut <- asNamespace('utils')$.read_authors_at_R_field(aar)
        vl <- format(aut, include = c("given", "family"))
        Author <- paste(unlist(vl), collapse=", ")
    }
    cbind(Package, Author)
}
