#' Download a Bioconductor vignette
#'
#' The actual vignette path is available using
#' \code{\link{biocPkgList}}.
#'
#' @param vignettePath character(1) the additional path information to get to the vignette
#' @param destfile character(1) the file location to store the vignette
#' @param version chacter(1) such as "3.7", defaults to user version
#'
#' @importFrom utils download.file
#'
#' @return character(1) the filename of the downloaded vignette
#' 
#' @examples
#' x = biocPkgList()
#' tmp = getBiocVignette(x$vignettes[[1]][1])
#' tmp
#' 
#' \dontrun{
#' library(pdftools)
#' y = pdf_text(tmp)
#' y = paste(y,collapse=" ")
#' library(tm)
#' v = VCorpus(VectorSource(y))
#' library(magrittr)
#'
#' v <- v %>%
#'     tm_map(stripWhitespace) %>%
#'     tm_map(content_transformer(tolower)) %>%
#'     tm_map(removeWords, stopwords("english")) %>%
#'     tm_map(stemDocument)
#' dtm = DocumentTermMatrix(v)
#' inspect(DocumentTermMatrix(v,
#'     list(dictionary = as.character(x$Package))))
#'}
#' @export
getBiocVignette <- function(vignettePath,
                        destfile = tempfile(),
                        version = BiocManager::version()) {
    stopifnot(is.character(vignettePath) & length(vignettePath)==1)
    p = sprintf('https://bioconductor.org/packages/%s/bioc/%s',version,vignettePath)
    download.file(p,destfile)
    destfile
}
