#' Extract github user and repo name from github URL
#'
#' @param urls a \code{character()} vector of urls.
#'
#' @return a \code{data.frame} with four columns:
#'   \itemize{
#'     \item{url}{The original github URL}
#'     \item{user_repo}{The github "username/repo", combined}
#'     \item{user}{The github username}
#'     \item{repo}{The github repo name}
#'   }
#'
#' @importFrom stringr str_match
#'
#' @examples
#' # find github URL details for
#' # Bioc packages
#' bpkgl = biocPkgList()
#' urldetails = githubURLParts(bpkgl$URL)
#' urldetails = urldetails[!is.na(urldetails$url),]
#' head(urldetails)
#'
#' @export
githubURLParts = function(urls) {
    tmp = str_match(urls,'https://github.com/(([^/]+)/([^/]+)).*')
    tmp = data.frame(tmp, stringsAsFactors = FALSE)
    colnames(tmp) = c('url', 'user_repo', 'user', 'repo')
    tmp
}
