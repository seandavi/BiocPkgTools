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
    rid <- BiocFileCache::bfcquery(bfc, "user.info", "rname", exact = TRUE)$rid
    if (!length(rid))
        userfile <- BiocFileCache::bfcnew(bfc, "user.info", ext = ".txt")
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

.getMailLog <- function() {
    bfc <- .get_cache()
    rid <- BiocFileCache::bfcquery(bfc, "email.log", "rname", exact = TRUE)$rid
    if (!length(rid)) {
        logpath <- BiocFileCache::bfcnew(bfc, "email.log", ext = ".rda")
        metainfo <- data.frame(maintainer = character(), email = character(),
            package = character(), dateSent = character(),
            stringsAsFactors = FALSE)
        save(metainfo, file = logpath)
    } else {
        logpath <- BiocFileCache::bfcrpath(bfc, rids = rid)
    }
    logpath
}

.checkEntry <- function(logloc, mainname, mainemail, pkg, date, dry.run) {
    if (dry.run) return(FALSE)
    mailinfo <- data.frame(maintainer = mainname, email = mainemail,
        package = pkg, dateSent = date, stringsAsFactors = FALSE)

    dataenv <- new.env(parent = emptyenv())
    load(logloc, dataenv)
    metainfo <- dataenv[["metainfo"]]
    newinfo <- do.call(rbind.data.frame, list(metainfo, mailinfo))

    anyDuplicated(newinfo)
}

.addEntry <- function(logloc, mainname, mainemail, pkg, date) {
    mailinfo <- data.frame(maintainer = mainname, email = mainemail,
        package = pkg, dateSent = date, stringsAsFactors = FALSE)

    dataenv <- new.env(parent = emptyenv())
    load(logloc, dataenv)
    metainfo <- dataenv[["metainfo"]]
    metainfo <- do.call(rbind.data.frame, list(metainfo, mailinfo))

    save(metainfo, file = logloc)
}

#' @rdname biocBuildEmail
#'
#' @title Create and copy e-mail package notification template to clipboard
#'
#' @description
#'     The \code{biocBuildEmail} function provides a template for notifying
#' maintainers of errors in the Bioconductor Build System (BBS). This
#' convenience function returns the body of the email from a template
#' within the package and provides a copy in the clipboard.
#'
#' @param pkg character(1) The name of the package in trouble
#'
#' @param PS character(1) Postscript, an additional note to the recipient of
#'     the email (i.e., the package maintainer)
#'
#' @param emailTemplate character(1) The path to the email template.
#'
#' @param dry.run logical(1) Display the email without sending to the recipient.
#'     It only works for HTML email reports and ignored when `textOnly=TRUE`
#'
#' @inheritParams biocBuildReport
#'
#' @return a character string of the email
#'
#' @export
biocBuildEmail <-
    function(pkg, version = c("release", "devel"), PS = character(1L),
        emailTemplate = .getTemplatePath(), core.name = NULL,
        core.email = NULL, core.id = NULL, textOnly = FALSE,
        dry.run = TRUE, resend = FALSE)
{
    stopifnot(
        is.character(pkg), identical(length(pkg), 1L),
        is.character(PS), identical(length(PS), 1L),
        !is.na(pkg), !is.na(PS),
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

    mainName <- unname(vapply(mainInfo, .nameCut, character(1L)))
    mainEmail <- unname(vapply(mainInfo, .emailCut, character(1L)))

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

    if (nchar(PS))
        PS <- paste0("P.S. ", PS)

    firstname <- vapply(strsplit(core.name, "\\s"), `[`, character(1L), 1L)
    mail <- paste0(readLines(emailTemplate), collapse = "\n")
    maildate <- format(Sys.time(), "%B %d, %Y")
    send <- sprintf(mail, pkg, core.name, maildate, pkg, mainName, pkg,
        vers, repolink, PS, firstname)

    title <- sprintf("%s Bioconductor package", pkg)
    logfile <- .getMailLog()
    sent_status <- .checkEntry(logfile, mainName, mainEmail, pkg, maildate, dry.run)

    if (dry.run)
        message("Message not sent: Set 'dry.run=FALSE'")

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
        if (!dry.run && !sent_status) {
            blastula::smtp_send(email = biocmail,
                from = core.email, to = mainEmail, subject = title,
                credentials =
                    blastula::creds(
                        user = paste0(core.id, "@roswellpark.org"),
                        provider = "office365",
                        sender_name = core.name,
                        use_ssl = FALSE
                    )
            )
            .addEntry(logfile, mainName, mainEmail, pkg, maildate)
        }
        return(biocmail)
    }
}

#' @name biocBuildEmail
#'
#' @section sentHistory: Check the history of emails sent
#'
#' @export
sentHistory <- function() {
    bfc <- .get_cache()
    ## first build data.frame logger
    rid <- BiocFileCache::bfcquery(bfc, "email.log", "rname", exact = TRUE)$rid
    if (!length(rid))
        stop("No log available. Send some emails.")

    minienv <- new.env(parent = emptyenv())
    load(BiocFileCache::bfcrpath(bfc, rids = rid), env = minienv)
    minienv[["metainfo"]]
}
