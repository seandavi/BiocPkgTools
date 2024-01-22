checkMe <- function(ver="devel", authorPattern="V.*Carey", includeOK = FALSE) {
 rep = biocBuildReport(ver)
 kp = grep(authorPattern, rep$author)
 mine = rep[kp,]
 if(nrow(mine) == 0L) stop("Used authorPattern return zero results.",
                           call. = FALSE)
 if(includeOK)
     bad = mine
 else
     bad = mine[mine$result != "OK",]
 if (nrow(bad)>0) return(bad[,-c(3,4,5)]) else return(NULL)
}

chkURL <- function(ver="devel", result = "ERROR", pack="BiocOncoTK",
   node="malbec1", stage="buildsrc"){
  urls <- rep("",length(ver))
  skipped <- result %in% c("skipped","NA")
  urls[!skipped] <- sprintf("https://bioconductor.org/checkResults/%s/bioc-LATEST/%s/%s-%s.html",
                            ver[!skipped], pack[!skipped], node[!skipped], stage[!skipped])
  urls[skipped] <- sprintf("https://bioconductor.org/checkResults/%s/bioc-LATEST/%s/",
                           ver[skipped], pack[skipped])
  urls
}

checkDeps <- function(dependsOn, ver="devel", includeOK = FALSE) {

  if(missing(dependsOn)) {
    stop("You must provide a package name to the 'dependsOn' argument")
  }

  rep = biocBuildReport(ver)
  all_pkg_deps = buildPkgDependencyDataFrame()
  pkg_deps <- all_pkg_deps[ all_pkg_deps$dependency == dependsOn, 1]

  mine = rep[rep$pkg %in% pkg_deps$Package, ]

  if(nrow(mine) == 0L) stop("No dependent packages found.",
                            call. = FALSE)
  if(includeOK)
    bad = mine
  else
    bad = mine[mine$result != "OK",]

  if (nrow(bad)>0) return(bad[,-c(3,4,5)]) else return(NULL)
}

#' generate hyperlinked HTML for build reports for Bioc packages
#'
#' This is a quick way to get an HTML report of packages maintained by a specific developer
#' or which depend directly on a specified package. The function is keyed to filter based on either
#' the maintainer name or by using the 'Depends', 'Suggests' and 'Imports' fields in package descriptions.
#'
#' @importFrom htmltools a
#' @importFrom DT datatable
#'
#' @param authorPattern character(1) regexp used with grep() to filter author field of package DESCRIPTION for listing
#' @param dependsOn character(1) name of a Bioconductor package. The function will return the status of packages
#' that directly depend on this package  Can only be used when 'authorPattern' is the empty string.
#' @param ver character(1) version tag for Bioconductor
#' @param includeOK logical(1) include entries from the build report that are listed
#'     as "OK". Default FALSE will result in only those entries that
#'     are in WARNING or ERROR state.
#'
#' @return DT::datatable call; if assigned to a variable, must evaluate to get the page to appear
#'
#' @author Vince Carey, Mike L. Smith
#'
#' @examples
#' if (interactive()) {
#'   problemPage()
#'   problemPage(dependsOn = "limma")
#' }
#'
#' @export
problemPage <- function(
    authorPattern="V.*Carey", dependsOn, ver="devel", includeOK = FALSE
) {

    if (!requireNamespace("htmltools")) stop("install htmltools to use this function")
    if (!requireNamespace("DT")) stop("install DT to use this function")

    if(nchar(authorPattern) > 0 && !missing(dependsOn)) {
      warning("Both 'authorPattern' and 'dependsOn' arguments provided\n",
              "Listing results for 'authorPattern'.")
      dependsOn = ""
    }

    ver = as.character(ver)

    if(nchar(authorPattern) > 0)
      mm = checkMe(authorPattern=authorPattern, ver=ver, includeOK = includeOK)
    else
      mm = checkDeps(dependsOn = dependsOn, ver=ver, includeOK = includeOK)

    nn = nrow(mm)
    if (is.null(nn)) stop("all packages fine")
    cc = chkURL(mm[["bioc_version"]], mm[["result"]], mm[["pkg"]], mm[["node"]],
                mm[["stage"]])
    hr = lapply(seq_len(nrow(mm)), function(x)
        htmltools::a(mm[x, "pkg"], href=cc[[x]]))
    col1 = unlist(lapply(hr, as.character))
    DT::datatable(data.frame(package = col1, mm[,-1], stringsAsFactors=FALSE),
                  escape=FALSE)
}
