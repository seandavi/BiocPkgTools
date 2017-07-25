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
#' @export
githubURLParts = function(urls) {
  tmp = as.data.frame(str_match(urls,'https://github.com/((.*)/(.*))'))
  colnames(tmp) = c('url', 'user_repo', 'user', 'repo')
}
