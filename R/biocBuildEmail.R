.nameCut <- function(string) {
    gsub("\n", " ",
        gsub("(.*)\\s+<.*", "\\1", string),
    fixed = TRUE
    )
}

.emailCut <- function(string) {
    gsub(" at ", "@",
        gsub("(.*)<(.*)>.*", "\\2", string)
    )
}

.getTemplatePath <- function() {
    system.file(
        package = "BiocPkgTools", "resources",
        "e-template.txt", mustWork = TRUE
    )
}

#' Create and copy e-mail package notification template to clipboard
#'
#' The \code{biocBuildEmail} function provides a template for notifying
#' maintainers of errors in the Bioconductor Build System (BBS). This
#' convenience function returns the body of the email from a template
#' within the package and provides a copy in the clipboard.
#'
#' @param pkg The name of the package in trouble
#' @param emailTemplate The path to the email template.
#' 
#' @inheritParams biocBuildReport
#' 
#' @return a character string of the email
#'
#' @export
biocBuildEmail <-
    function(pkg, version = BiocManager::version(),
        emailTemplate = .getTemplatePath())
{
    stopifnot(
        is.character(pkg),
        identical(length(pkg), 1L),
        file.exists(emailTemplate)
    )

    if (!requireNamespace("clipr", quietly = TRUE))
        stop("Install the 'clipr' package to use 'biocBuildEmail'")

    listall <- biocPkgList(version = version)
    pkgMeta <- listall[listall[["Package"]] %in% pkg, ]
    if(nrow(pkgMeta) == 0L) stop("No pkg '",pkg,"' found on Bioconductor for ",
                                 "version '",version,"'")
    mainInfo <- pkgMeta[["Maintainer"]][[1L]]

    mainName <- vapply(mainInfo, .nameCut, character(1L))
    mainEmail <- vapply(mainInfo, .emailCut, character(1L))

    if (length(mainName) > 1L)
        mainName <- paste0(mainName, collapse = " & ")
    if (length(mainEmail) > 1L)
        mainEmail <- paste0(mainEmail, collapse = ", ")

    mail <- paste0(readLines(emailTemplate), collapse = "\n")
    send <- sprintf(mail, mainName, mainEmail, pkg, version, version, pkg)

    if (clipr::clipr_available()) {
        clipr::write_clip(send)
        message("Message copied to clipboard")
    } else
        message("Unable to put result on the clipboard")

    send
}
