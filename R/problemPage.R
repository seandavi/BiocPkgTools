checkMe = function(ver="devel", authorPattern="V.*Carey", includeOK = FALSE) {
 rep = biocBuildReport(ver)
 kp = grep(authorPattern, rep$author)
 mine = rep[kp,]
 if(includeOK)
     bad = mine
 else
     bad = mine[mine$result != "OK",]
 if (nrow(bad)>0) return(bad[,-c(3,4,5)]) else return(NULL)
}
 
chkURL = function(ver="devel", result = "ERROR", pack="BiocOncoTK",
   node="malbec1", stage="buildsrc"){
  urls <- rep("",length(ver))
  skipped <- result %in% c("skipped","NA")
  urls[!skipped] <- sprintf("https://bioconductor.org/checkResults/%s/bioc-LATEST/%s/%s-%s.html", 
                            ver[!skipped], pack[!skipped], node[!skipped], stage[!skipped])
  urls[skipped] <- sprintf("https://bioconductor.org/checkResults/%s/bioc-LATEST/%s/", 
                           ver[skipped], pack[skipped])
  urls
}

#' generate hyperlinked HTML for build reports for Bioc packages
#'
#' This is a quick way to get an HTML report of a developer's packages.
#' The function is keyed to filter based on maintainer name.
#' 
#' @importFrom htmltools a
#' @importFrom DT datatable
#'
#' @param authorPattern character(1) regexp used with grep() to filter author field of package DESCRIPTION for listing
#' @param ver character(1) version tag for Bioconductor
#' @param includeOK logical(1) include entries from the build report that are listed
#'     as "OK". Default FALSE will result in only those entries that
#'     are in WARNING or ERROR state.
#'
#' @return DT::datatable call; if assigned to a variable, must evaluate to get the page to appear
#'
#' @author Vince Carey
#' 
#' @examples
#' if (interactive()) problemPage()
#'
#' @export
problemPage = function(authorPattern="V.*Carey", ver="devel", includeOK = FALSE) {
    if (!requireNamespace("htmltools")) stop("install htmltools to use this function")
    if (!requireNamespace("DT")) stop("install DT to use this function")
    ver = as.character(ver)
    mm = checkMe(authorPattern=authorPattern, ver=ver, includeOK = includeOK)
    nn = nrow(mm)
    if (is.null(nn)) stop("all packages fine")
    cc = chkURL(mm[["bioc_version"]], mm[["result"]], mm[["pkg"]], mm[["node"]],
                mm[["stage"]])
    hr = lapply(seq_len(nrow(mm)), function(x)
        htmltools::a(mm[x, "pkg"], href=cc[[x]]))
    col1 = unlist(lapply(hr, as.character))
    DT::datatable(data.frame(col1, mm[,-1], stringsAsFactors=FALSE), escape=FALSE)
}
