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

.getUserInfo <- function(core.name, core.email, core.id) {
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
            core.id <- readline(paste0("What is your Roswell Park employee ID",
                " (matches ^[A-Z]{2}[0-9]{5})?  "))

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
            package = character(), dateSent = character(), times = integer(),
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
        package = pkg, dateSent = date, times = 1L, stringsAsFactors = FALSE)

    dataenv <- new.env(parent = emptyenv())
    load(logloc, dataenv)
    metainfo <- dataenv[["metainfo"]]
    newinfo <- do.call(rbind.data.frame, list(metainfo, mailinfo))

    anyDuplicated(newinfo[, names(newinfo) != "times"])
}

.addEntry <- function(logloc, mainname, mainemail, pkg, date, resend) {
    mailinfo <- data.frame(maintainer = mainname, email = mainemail,
        package = pkg, dateSent = date, times = 1L, stringsAsFactors = FALSE)

    dataenv <- new.env(parent = emptyenv())
    load(logloc, dataenv)
    metainfo <- dataenv[["metainfo"]]

    impcols <- names(metainfo)[names(metainfo) != "times"]
    bound <- do.call(rbind.data.frame, list(metainfo, mailinfo))

    if (resend) {
        dup <- duplicated(bound[, impcols], fromLast = TRUE)
        if (!any(dup))
            stop("'resend' used in the wrong context")
        dup <- dup[-length(dup)]
        metainfo[dup, "times"] <- metainfo[dup, "times"] + 1L
    } else {
        metainfo <- do.call(rbind.data.frame, list(metainfo, mailinfo))
    }

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
#' @param version character() A vector indicating which version of Bioconductor
#'     the package is failing in (either 'release' or 'devel'; defaults to both)
#'
#' @param PS character(1) Postscript, an additional note to the recipient of
#'     the email (i.e., the package maintainer)
#'
#' @param dry.run logical(1) Display the email without sending to the recipient.
#'     It only works for HTML email reports and ignored when `textOnly=TRUE`
#'
#' @param mainIndex numeric()
#'
#' @param emailTemplate character(1) The path to the email template. The default
#'     path lies in the 'inst' package folder.
#'
#' @param core.name character(1) The full name of the core team member
#'
#' @param core.email character(1) The Roswell Park email of the core team
#'     member
#'
#' @param core.id character(1) The internal identifier for the Roswell employee.
#'     This ID usually matches `^[A-Z]{2}[0-9]{5}` for more recent identifiers.
#'
#' @param textOnly logical(1) Whether to return the text of the email only.
#'     This avoids the use of the 'blastula' package and adds the text to the
#'     system clipboard if the `clipr` package is installed (default: FALSE)
#'
#' @param resend logical(1) Whether to force a resend of the email
#'
#' @param verbose logical(1) Whether to output full email information from
#'     'smtp_send' (when `dry.run` is `FALSE` and 'blastula' is installed)
#'
#' @param credFile character(1) An optional file generated by the
#'     `blastula::create_smtp_creds_file` function containing email auth info
#'      (default: "~/.blastula_creds").
#'
#' @return A character string of the email
#'
#' @export
biocBuildEmail <-
    function(pkg, version = c("release", "devel"), PS = character(1L),
        dry.run = TRUE, mainIndex = NULL, emailTemplate = .getTemplatePath(),
        core.name = NULL, core.email = NULL, core.id = NULL,
        textOnly = FALSE, resend = FALSE, verbose = FALSE,
        credFile = "~/.blastula_creds")
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

    listall <- biocPkgList(version = version)
    pkgMeta <- listall[listall[["Package"]] %in% pkg, ]
    if(nrow(pkgMeta) == 0L) stop("No pkg '",pkg,"' found on Bioconductor for ",
                                 "version '",version,"'")

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
    sent_status <- .checkEntry(logfile, mainName, mainEmail, pkg, maildate,
        dry.run)
    sendagain <- (sent_status && resend)

    if (dry.run)
        message("Message not sent: Set 'dry.run=FALSE'")

    if (textOnly) {
        send <- strsplit(send, "---")[[1L]][[4L]]
        send <- paste(mainEmail, title, send, sep = "\n")
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
        if (!dry.run && (!sent_status || sendagain)) {
            blastula::smtp_send(email = biocmail,
                from = core.email, to = mainEmail, subject = title,
                credentials =
                    if (file.exists(credFile)) {
                        blastula::creds_file(credFile)
                    } else {
                        blastula::creds(
                            user = paste0(core.id, "@roswellpark.org"),
                            host = "smtp.office365.com",
                            port = 587,
                            use_ssl = TRUE
                        )
                    }, verbose = verbose
            )
            .addEntry(logfile, mainName, mainEmail, pkg, maildate, sendagain)
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
    load(BiocFileCache::bfcrpath(bfc, rids = rid), envir = minienv)
    minienv[["metainfo"]]
}
