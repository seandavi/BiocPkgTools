checkMe = function(ver="3.8", authorPatt="V.*Carey") {
 rep = bioc_build_report(ver)
 kp = grep(authorPatt, rep$author)
 mine = rep[kp,]
 print(dim(mine))
 bad = mine[mine$result != "OK",]
 if (nrow(bad)>0) return(bad[,-c(3,4,5)]) else return(NULL)
}
 
chkURL = function(ver="3.8", pack="BiocOncoTK",
   node="malbec1", stage="buildsrc")
  sprintf("http://bioconductor.org/checkResults/%s/bioc-LATEST/%s/%s-%s.html", ver, pack, node, stage)

#' generate hyperlinked HTML for problematic build reports for Bioc packages
#' @importFrom htmltools a
#' @importFrom DT datatable
#' @param authorPatt character(1) regexp used with grep() to filter author field of package DESCRIPTION for listing
#' @param ver character(1) version tag for Bioconductor
#' @return DT::datatable call; if assigned to a variable, must evaluate to get the page to appear
#' @examples
#' if (interactive()) problemPage()
#' @export
problemPage = function(authorPatt="V.*Carey", ver="3.8") {
 if (!requireNamespace("htmltools")) stop("install htmltools to use this function")
 if (!requireNamespace("DT")) stop("install DT to use this function")
 mm = checkMe(authorPatt=authorPatt, ver=ver)
 nn = nrow(mm)
 if (is.null(nn)) stop("all packages fine")
 cc = chkURL(mm[,"bioc_version"], mm[,"pkg"], mm[,"node"], mm[,"stage"])
 hr = lapply(1:nrow(mm), function(x)
    htmltools::a(mm[x, "pkg"], href=cc[[x]]))
 col1 = unlist(lapply(hr, as.character))
 DT::datatable(data.frame(col1, mm[,-1], stringsAsFactors=FALSE), escape=FALSE)
}