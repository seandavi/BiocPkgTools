#' Generate needed inforamation to create DOI from a package
#' DESCRIPTION file
#'
#' @param file character(1) Path to DESCRIPTION file
#'
#' @return a \code{data.frame}
#'
#' @import desc
#'
#' @export
getPackageInfo <- function(file){

    fnd = grep("Authors@R", readLines(file))
    
    if(length(fnd) == 0L){
  # if Author present not Authors@R
        desc = read.dcf(file, fields=c("Package", "Author"))
        desc
    }else{
 # if Authors@R present
        require(desc)
        desc =  description$new(file)
        aut = desc$get_authors()
        vl = lapply(aut, FUN=function(name){
            tmp = strsplit(as.character(name), split="\\s+")[[1]]
            paste(tmp[1], tmp[2])
        })
        Author = paste(unlist(vl), collapse=", ")
        Package = unname(desc$get("Package"))
        cbind(Package, Author)
    }
}
