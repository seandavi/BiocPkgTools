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

.getTemplatePath <- function(type = c("buildemail", "cranreport")) {
    type <- match.arg(type)
    type <- switch(
        type,
        buildemail = "BiocBuildEmail_Template.Rmd",
        cranreport = "CRANReport_Template.Rmd"
    )
    system.file(
        package = "BiocPkgTools", "resources", type,
        mustWork = TRUE
    )
}

.getUserInfo <- function(core.name = NULL, core.email = NULL, core.id = NULL) {
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
        if (is.null(core.id))
            core.id <- readline("What is your Roswell Park employee ID (matches ^[A-Z]{2}[0-9]{5})?  ")

        writeLines(c(core.name, core.email, core.id), con = userfile)
        message("Saved data to: ", pkgToolsCache())
    } else {
        devinfo <- readLines(userfile)
        core.name <- devinfo[[1L]]
        core.email <- devinfo[[2L]]
        core.id <- devinfo[[3L]]
    }
    list(core.name = core.name, core.email = core.email, core.id = core.id)
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
        core.email = NULL, core.id = NULL, textOnly = FALSE)
{
    stopifnot(
        is.character(pkg),
        identical(length(pkg), 1L),
        !is.na(pkg),
        file.exists(emailTemplate)
    )

    if (!textOnly) {
        if (!requireNamespace("blastula"))
            stop("Install the 'blastula' package to send HTML emails or use\n",
                "  'textOnly=TRUE'")
    }

    core.list <- .getUserInfo(core.name, core.email, core.id)
    core.name <- core.list[["core.name"]]
    core.email <- core.list[["core.email"]]
    core.id <- core.list[["core.id"]]

    stopifnot(
        is.character(core.name), is.character(core.email), is.character(core.id),
        !is.na(core.name), !is.na(core.email), !is.na(core.id),
        nchar(core.name) > 4, nchar(core.email) != 0, nchar(core.id) > 6
    )

    coredomain <- grepl("@roswellpark.org$", ignore.case = TRUE, x = core.email)
    if (!coredomain)
        stop("Provide only a core team email address")

    if (textOnly && !requireNamespace("clipr", quietly = TRUE))
        stop(paste0("Install the 'clipr' package to use the 'textOnly = TRUE'"))

    listall <- biocPkgList()
    pkgMeta <- listall[listall[["Package"]] == pkg, ]
    if (!nrow(pkgMeta))
        stop("Package not found in Bioconductor repository")
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
    maildate <- format(Sys.time(), "%B %d, %Y")
    send <- sprintf(mail, pkg, core.name, maildate, pkg, mainName, pkg,
        vers, repolink, firstname, core.name)

    title <- sprintf("%s Bioconductor package", pkg)

    if (textOnly) {
        send <- strsplit(send, "---")[[1L]][[4L]]
        if (clipr::clipr_available()) {
            clipr::write_clip(send)
            message("Message copied to clipboard")
        } else
            message("Unable to put result on the clipboard")
        return(send)
    } else {
        tfile <- tempfile(fileext = ".Rmd")
        writeLines(send, tfile)
        biocmail <- blastula::render_email(tfile)
        blastula::smtp_send(email = biocmail,
            from = core.email, to = mainEmail, subject = title,
            credentials = creds(
                user = paste0(core.id, "@roswellpark.org"),
                provider = "office365",
                sender_name = core.name,
                use_ssl = FALSE
            )
        )
        return(biocmail)
    }
}
