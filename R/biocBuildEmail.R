.nameCut <- function(str) {
    gsub("(.*)\\s+<.*", "\\1", str)
}

.getOnlineMainInfo <- function(pkg, version = BiocManager::version()) {
    doc <- xml2::read_html(
        sprintf(
            "http://bioconductor.org/packages/%s/bioc/html/%s.html",
            version,
            pkg
        )
    )
    maint <- grep("Maintainer:",
        rvest::html_text(rvest::html_nodes(doc, "p")), value = TRUE)
    email <- gsub("(.*)<(.*)>.*", "\\2", maint)
    name <- gsub("Maintainer: ", "", maint)

    list(name = .nameCut(name), email = gsub(" at ", "@", email))
}

.getInstMainInfo <- function(pkg) {
    maint <- utils::maintainer(pkg)
    list(name = .nameCut(maint), email = gsub("(.*)<(.*)>", "\\2", maint))
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

    maintainer <-
        if (pkg %in% rownames(installed.packages()))
            .getInstMainInfo(pkg)
        else
            .getOnlineMainInfo(pkg, version)

    mail <- paste0(readLines(emailTemplate), collapse = "\n")
    send <- sprintf(mail,
        maintainer[["name"]], maintainer[["email"]], pkg, version, version, pkg)

    if (clipr::clipr_available()) {
        clipr::write_clip(send)
        message("Message copied to clipboard")
    } else
        message("Unable to put result on the clipboard")

    send
}
