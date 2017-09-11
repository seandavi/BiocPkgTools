
#' Generate a DOI for a bioconductor package
#' 
#' This function makes calls out to the EZID API (v2) described
#' here: \url{https://ezid.lib.purdue.edu/doc/apidoc.2.html}. The 
#' function creates a new DOI for a bioc package (cannot already 
#' exist). The target URL for the DOI is the short Bioconductor
#' package URL.
#' 
#' The login information for the "real" Bioconductor account 
#' should be stored in the environment variables "EZID_USERNAME"
#' and "EZID_PASSWORD".
#' 
#' The GUI is available here: \url{https://ezid.lib.purdue.edu}.
#' 
#' @param pkg character(1) package name
#' @param authors character vector of authors (will be "pasted" together)
#' @param pubyear integer(1) publication year
#' @param testing logical(1) If true, will use the apitest
#'     user with the password apitest. These DOIs will expire. 
#'     The same apitest:apitest combination can be used to
#'     login to the EZID website for doing things using the
#'     web interface. If false, the Bioconductor-specific
#'     user credentials should be in the correct environment
#'     variables
#'     
#' @return The DOI as a character(1) vector.
#'     
#' @examples 
#' \dontrun{
#'   x = generateBiocPkgDOI('RANDOM_TEST_PACKAGE','Sean Davis',1972)
#' }
#' @export
generateBiocPkgDOI = function(pkg, authors, pubyear, testing=TRUE) {
  if(testing) {
    username='apitest'
    password='apitest'
    bioc_shoulder='doi:10.5072/FK2'
  } else {
    username=Sys.getenv('EZID_USERNAME')
    password=Sys.getenv('EZID_PASSWORD')
    bioc_shoulder='doi:10.18129/B9'
  }
  require(httr)
  base_url = "https://ezid.cdlib.org/id"
  bioc_doi_namespace = ".bioc."
  pkg_doi = paste0(bioc_shoulder,bioc_doi_namespace,pkg)
  url0 = file.path(base_url,pkg_doi)
  message(url0)
  body = paste(c(sprintf("datacite.title: %s",pkg),
                 sprintf("_target: https://bioconductor.org/packages/%s",pkg),
                 sprintf("datacite.creator: %s",gsub('\n','',paste(authors,collapse=", "))),
                 "datacite.publisher: Bioconductor",
                 sprintf("datacite.publicationyear: %d",pubyear),
                 sprintf("datacite.resourcetype: %s","Software")),collapse="\n")
  res = httr::POST(url0,
                  content_type('text/plain'),
                  accept("text/plain"),
                  httr::authenticate(username,password),
                  body=body,timeout(30))
  if(status_code(res)==400) {
    res = httr::PUT(url0,
                     content_type('text/plain'),
                     accept("text/plain"),
                     httr::authenticate(username,password),
                     body=body,timeout(30))
  }
  message(res)
  return(res)
  tmp = strsplit(content(res),' ')[[1]][c(2,4)]
  return(tmp[1])
}
