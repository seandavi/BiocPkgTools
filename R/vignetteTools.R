#' Get vignette
#'
#' @param vignettePath the additional path information to get to the vignette
#' @param destfile the file location to store the vignette
#' @param biocVersion defaults to user version
#'
#' @importFrom utils download.file
#' 
#' @examples
#' \dontrun{
#' x = getBiocPkgList()
#' tmp = getVignette(x$vignettes[[1]][1])
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
getVignette <- function(vignettePath,
                        destfile = tempfile(),
                        biocVersion = BiocManager::version()) {
    p = sprintf('https://bioconductor.org/packages/%s/bioc/%s',biocVersion,vignettePath)
    download.file(p,destfile)
    destfile
}
