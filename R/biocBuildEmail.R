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
    function(pkg, version = c("release", "devel"),
        emailTemplate = .getTemplatePath(), core.name = NULL,
        core.email = NULL)
{
    stopifnot(
        is.character(pkg),
        identical(length(pkg), 1L),
        file.exists(emailTemplate)
    )

    bfc <- .get_cache()
    rid <- BiocFileCache::bfcquery(bfc, "userinfo", "rname", exact = TRUE)$rid
    if (!length(rid))
        userfile <- BiocFileCache::bfcnew(bfc, "userinfo", ext = ".txt")
    else
        userfile <- BiocFileCache::bfcrpath(bfc, rids = rid)

    if (!file.exists(userfile)) {
        if (is.null(core.name))
            core.name <- readline("Provide your full name: ")
        if (is.null(core.email))
            core.email <- readline("What is your core-team email? ")

        writeLines(c(core.name, core.email), con = userfile)
        message("Saved data to: ", pkgToolsCache())
    } else {
        devinfo <- readLines(userfile)
        core.name <- devinfo[[1L]]
        core.email <- devinfo[[2L]]
    }

    stopifnot(
        is.character(core.name), is.character(core.email),
        !is.na(core.name), !is.na(core.email),
        nchar(core.name) > 4, nchar(core.email) != 0
    )

    coredomain <- grepl("@roswellpark.org$", ignore.case = TRUE, x = core.email)
    if (!coredomain)
        stop("Provide only a core team email address")

    if (!requireNamespace("clipr", quietly = TRUE))
        stop("Install the 'clipr' package to use 'biocBuildEmail'")

    listall <- biocPkgList()
    pkgMeta <- listall[listall[["Package"]] == pkg, ]
    mainInfo <- pkgMeta[["Maintainer"]][[1L]]

    mainName <- vapply(mainInfo, .nameCut, character(1L))
    mainEmail <- vapply(mainInfo, .emailCut, character(1L))

    if (length(mainName) > 1L)
        mainName <- paste0(mainName, collapse = " & ")
    if (length(mainEmail) > 1L)
        mainEmail <- paste0(mainEmail, collapse = ", ")

    if (length(version) == 2L)
        vers <- paste(version, collapse = " and ")
    else
        vers <- version

    repolink <- vapply(version, function(vername) {
        sprintf("https://bioconductor.org/checkResults/%s/bioc-LATEST/%s/",
            vername, pkg)
    }, character(1L))
    repolink <- paste0(repolink, collapse = "\n")

    firstname <- vapply(strsplit(core.name, "\\s"), `[`, character(1L), 1L)
    mail <- paste0(readLines(emailTemplate), collapse = "\n")
    send <- sprintf(mail, mainName, mainEmail, pkg, vers, repolink,
        firstname, core.name)

    if (clipr::clipr_available()) {
        clipr::write_clip(send)
        message("Message copied to clipboard")
    } else
        message("Unable to put result on the clipboard")

    send
}
