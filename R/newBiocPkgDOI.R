
#' Generate a DOI for a Bioconductor package
#'
#' This function makes calls out to the DataCite REST API described
#' here: \url{https://support.datacite.org/docs/api-create-dois}. The
#' function creates a new DOI for a Bioconductor package (cannot already
#' exist). The target URL for the DOI is the short Bioconductor
#' package URL.
#'
#' The login information for the "real" Bioconductor account
#' should be stored in the environment variables "DATACITE_USERNAME"
#' and "DATACITE_PASSWORD
#'
#' The GUI is available here: \url{https://doi.datacite.org/}.
#'
#' @param pkg character(1) package name
#' @param authors character vector of authors (will be "pasted" together)
#' @param pubyear integer(1) publication year
#' @param event Either "hide", "register", or publish". Typically, we use
#'     "publish" to make the DOI findable.
#' @param testing logical(1) If true, will use the apitest
#'     user with the password apitest. These DOIs will expire.
#'     The same apitest:apitest combination can be used to
#'     login to the website for doing things using the
#'     web interface. If false, the Bioconductor-specific
#'     user credentials should be in the correct environment
#'     variables
#'
#' @return The DOI as a character(1) vector.
#'
#' @importFrom httr VERB content_type content add_headers message_for_status
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stringr str_to_upper
#'
#' @keywords Internal
#'
#' @examples
#' \dontrun{
#'   x = generateBiocPkgDOI('RANDOM_TEST_PACKAGE','Sean Davis',1972)
#' }
generateBiocPkgDOI <- function(pkg, authors, pubyear, event = "publish", testing = TRUE) {

  username <- Sys.getenv("DATACITE_USERNAME")
  password <- Sys.getenv("DATACITE_PASSWORD")

  if (!is.element(event, c("hide", "register", "publish")))
    stop("event must be 'hide', 'register', or 'publish'.")

  if (testing) {
    # View results at: https://doi.test.datacite.org
    bioc_prefix <- "10.82962"
    base_url <- "https://api.test.datacite.org/dois"
  } else {
    bioc_prefix <- "10.18129"
    base_url <- "https://api.datacite.org/dois"
  }

  bioc_doi_namespace <- "B9.bioc"
  pkg_doi <- paste0(bioc_prefix, "/", bioc_doi_namespace, ".", pkg)
  encode <- "raw"
  payload <- list("data" = list("id" = paste0("https://doi.org/", pkg_doi),
                                "doi" = stringr::str_to_upper(pkg_doi),
                                "attributes" = list("doi" = pkg_doi,
                                                    "event" = event,
                                                    "prefix" = bioc_prefix,
                                                    "suffix" = paste(bioc_doi_namespace, pkg, sep = "."),
                                                    "identifiers" = list("identifier" = pkg_doi,
                                                                         "identifierType" = "DOI"),
                                                    "creators" = list("name" = paste(authors, collapse = ", ")),
                                                    "titles" = list("title" = pkg),
                                                    "url" = paste0("https://bioconductor.org/packages/", pkg),
                                                    "publisher" = "Bioconductor",
                                                    "publicationYear" = pubyear,
                                                    "types" = list("resourceTypeGeneral" = "Software")
                                                   )
                                )
                 )

  authorization <- jsonlite::base64_enc(paste(username, password, sep = ":"))
  response <- httr::VERB("POST",
                         base_url,
                         body = jsonlite::toJSON(payload, auto_unbox = TRUE),
                         httr::add_headers(Authorization = paste("Basic", authorization, sep = " ")),
                         httr::content_type("application/vnd.api+json"),
                         encode = encode)

  if (response$status_code >= 200 && response$status_code < 300) {
    response_text <- httr::content(response, "text")
    response_json <- jsonlite::fromJSON(response_text)
    return(response_json$data$id)
  } else {
    httr::message_for_status(response)
  }
}
