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
    tmp = str_match(urls,'.*http[s]?://github.com/(([^/]+)/([^/]+)).*')
    tmp = data.frame(tmp, stringsAsFactors = FALSE)
    colnames(tmp) = c('url', 'user_repo', 'user', 'repo')
    ## remove ".git" from tail of github repo
    tmp$user_repo = sub('\\.git$','', tmp$user_repo)
    tmp$repo = sub('\\.git$','', tmp$repo)
    tmp
}


.gh_pkg_info = function(user_repo) {
    f = tryCatch(gh(sprintf('/repos/%s', user_repo)),
                 error=function(e) {
                     warning(sprintf("package %s not found", user_repo))
                     NA
                 })
    f[is.null(f)]=""
    f
}
    

#' Get package details from GitHub
#'
#' For packages that live on github, we can mine
#' further details. This function returns the
#' github details for the listed packages.
#'
#' @details
#' The \code{\link[gh]{gh}} function is used to
#' do the fetching. If the number of packages supplied
#' to this function is large (>40 or so), it is possible
#' to run into problems with API rate limits. The \code{gh}
#' package uses the environment variable "GITHUB_PAT"
#' (for personal access token) to authenticate and then
#' provide higher rate limits. If you run into problems
#' with rate limits, set sleep to some small positive
#' number to slow queries. Alternatively, create a Personal
#' Access Token on GitHub and register it. See the \code{gh}
#' package for details.
#'
#' @importFrom gh
#'
#' @examples
#' pkglist = biocPkgList()
#' gh_list = githubURLParts(pkglist$URL)
#' gh_list = gh_list[!is.null(gh_list$user_repo),]
#' ghd = githubDetails(gh_list$user_repo[1:5])
#' lapply(ghd, '[[', "stargazers")
#' 
#' @export
githubDetails = function(pkgs, sleep=0) {
    vals = lapply(pkgs, function(pkg) {
        Sys.sleep(sleep)
        .gh_pkg_info(pkg)
    })
    names(vals) = pkgs
    vals = vals[!is.na(vals)]
    vals
}
                  
