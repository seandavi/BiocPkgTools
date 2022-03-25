.createBiocPkgLink <- function(pkg, version = "devel", md = TRUE) {
    buildrepURL <- paste0(
        "https://bioconductor.org/checkResults/", version, "/bioc-LATEST/", pkg
    )
    if (md)
        paste0("[", pkg, "]", "(", buildrepURL, ")")
    else
        buildrepURL
}

#' @rdname biocRevDepEmail
#'
#' @title Notify downstream maintainers of changes in upstream package
#'
#' @description
#'     The `biocRevDepEmail` function collects all the emails of the reverse
#' dependencies and sends a notification that an upstream package has been
#' deprecated or removed. It uses a template found in `inst/resources` with
#' the `templatePath()` function.
#'
#' @param pkg character(1) The name of the package for whose reverse
#'     dependencies are to be checked and notified.
#'
#' @inheritParams biocBuildEmail
#' @inheritParams tools::package_dependencies
#'
#' @examples
#'
#' biocRevDepEmail(
#'     "FindMyFriends", version = "3.13", dry.run = TRUE, textOnly = TRUE
#' )
#'
#' @export
biocRevDepEmail <-
    function(pkg, which = c("strong", "most", "all"),
        PS = character(1L), version = BiocManager::version(),
        dry.run = TRUE,  cc = NULL, emailTemplate = templatePath("revdepnote"),
        core.name = NULL, core.email = NULL, core.id = NULL,
        textOnly = FALSE, verbose = FALSE, credFile = "~/.blastula_creds")
{
    stopifnot(
        is.character(pkg), identical(length(pkg), 1L),
        is.character(PS), identical(length(PS), 1L),
        !is.na(pkg), !is.na(PS), !is.na(core.name), !is.na(core.email),
        !is.na(core.id)
    )
    if (!file.exists(emailTemplate))
        stop("'emailTemplate' file not found.")
    which <- match.arg(which)

    core.list <- .getUserInfo(core.name, core.email, core.id)
    core.name <- core.list[["core.name"]]
    core.email <- core.list[["core.email"]]
    core.id <- core.list[["core.id"]]

    db <- available.packages(
        repos = BiocManager:::.repositories_bioc(version)[1]
    )
    revdeps <- tools::package_dependencies(
        pkg, db, reverse = TRUE, which = which
    )[[pkg]]

    if (!length(revdeps))
        stop("No reverse dependencies on ", pkg)

    listall <- biocPkgList(version = version)
    pkgMeta <- listall[listall[["Package"]] %in% revdeps, "Maintainer"]
    mainInfo <- vapply(
        unlist(pkgMeta, use.names = FALSE), .emailCut, character(1L)
    )

    mainEmails <- unname(vapply(mainInfo, .emailCut, character(1L)))

    repolink <- .createBiocPkgLink(pkg, version, FALSE)

    if (nchar(PS))
        PS <- paste0("**P.S.** ", PS)

    revdeps <- paste(.createBiocPkgLink(revdeps, version), collapse = "\n\n")
    mail <- paste0(readLines(emailTemplate), collapse = "\n")
    maildate <- format(Sys.time(), "%B %d, %Y")
    firstname <- vapply(strsplit(core.name, "\\s"), `[`, character(1L), 1L)
    send <- sprintf(mail, pkg, core.name, maildate, pkg, pkg, revdeps,
        repolink, firstname, PS)
    title <- sprintf("Bioconductor Package %s Deprecation Notification", pkg)

    if (dry.run)
        message("Message not sent: Set 'dry.run=FALSE'")

    if (textOnly) {
        send <- strsplit(send, "---")[[1L]][[4L]]
        mainEmails <- paste(mainEmails, collapse = ", ")
        send <- paste(mainEmails, title, send, sep = "\n")
        if (requireNamespace("clipr", quietly=TRUE) &&
            clipr::clipr_available())
        {
            clipr::write_clip(send)
            message("Message copied to clipboard")
        }
    } else {
        tfile <- tempfile(fileext = ".Rmd")
        writeLines(send, tfile)
        send <- blastula::render_email(tfile)
        if (!dry.run) {
            blastula::smtp_send(email = send,
                from = core.email, to = core.email, bcc = mainEmails, cc = cc,
                subject = title,
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
        }
    }
    send
}

