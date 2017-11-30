#' Generate needed inforamation to create DOI from a package
#' directory.
#'
#' @param dir character(1) Path to package
#'
#' @return a \code{data.frame}
#'
#' @export
getPackageInfo <- function(dir){
    db <- tools:::.read_description(file.path(dir, "DESCRIPTION"))
    Package <- unname(db["Package"])
    aar <- db["Authors@R"]
    if (is.na(aar)) {
        Author <- unname(db["Author"])
    } else {
        aut <- utils:::.read_authors_at_R_field(aar)
        vl <- lapply(aut, FUN=function(name){
            tmp = strsplit(as.character(name), split="\\s+")[[1]]
            paste(tmp[1], tmp[2])
        })
        Author <- paste(unlist(vl), collapse=", ")
    }
    cbind(Package, Author)
}
