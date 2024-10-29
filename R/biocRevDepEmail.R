.pkgsLinks <- function(pkgs, version, md = TRUE) {
    db <- available.packages(repos = BiocManager::repositories())
    pkgsdb <- db[rownames(db) %in% pkgs, , drop = FALSE]
    isbioc <- grepl("bioconductor", pkgsdb[, "Repository"], fixed = TRUE)
    c(
        .biocPkgsLinks(pkgs[isbioc], version = version, md = md),
        .CRANpkgsLinks(pkgs[!isbioc], md = md)
    )
}

.biocPkgsLinks <- function(pkgs, version, md = TRUE) {
    if (!length(pkgs))
        return(NULL)

    buildrepURL <- paste0(
        "https://bioconductor.org/checkResults/", version, "/bioc-LATEST/", pkgs
    )
    if (md)
        paste0("[", pkgs, "]", "(", buildrepURL, ")")
    else
        buildrepURL
}

.CRANpkgsLinks <- function(pkgs, md = TRUE) {
    if (!length(pkgs))
        return(NULL)

    cranrepURL <- paste0(
        .CRAN_WEB_URL, "package=", pkgs
    )
    if (md)
        paste0("[", pkgs, "]", "(", cranrepURL, ")")
    else
        cranrepURL
}

#' Notify downstream maintainers of changes in upstream packages
#'
#' @description
#' The `biocRevDepEmail` function collects all the emails of the reverse
#' dependencies and sends a notification that upstream package(s) have been
#' deprecated or removed. It uses a template found in `inst/resources` with the
#' `templatePath()` function.
#'
#' @param packages `character()` A vector of CRAN and/or Bioconductor packages
#'   for whose reverse dependencies are to be checked and notified.
#'
#' @param cc character() A vector of email addresses for sending the message
#'   as a carbon copy.
#'
#' @param pkg `character(1)` DEPRECATED. The name of a single package whose
#'   reverse dependencies are to be checked and notified.
#'
#' @param \ldots Additional inputs to internal functions (not used).
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
    function(packages, which = c("strong", "most", "all"),
        PS = character(1L), version = BiocManager::version(),
        dry.run = TRUE,  cc = NULL, emailTemplate = templatePath("revdepnote"),
        core.name = NULL, core.email = NULL, core.id = NULL,
        textOnly = FALSE, verbose = FALSE, credFile = "~/.blastula_creds",
        ..., pkg)
{
    stopifnot(
        is.character(packages),
        is.character(PS), identical(length(PS), 1L),
        !is.na(packages), !is.na(PS), !is.na(core.name), !is.na(core.email),
        !is.na(core.id)
    )
    if (!file.exists(emailTemplate))
        stop("'emailTemplate' file not found.")
    which <- match.arg(which)

    core.list <- .getUserInfo(core.name, core.email, core.id)
    core.name <- core.list[["core.name"]]
    core.email <- core.list[["core.email"]]
    core.id <- core.list[["core.id"]]

    if (!missing(pkg))
        .Defunct(msg = "'pkg' argument is defunct. Use 'packages'.")

    db <- available.packages(
        repos = BiocManager:::.repositories_bioc(version)["BioCsoft"]
    )
    revdeps <- tools::package_dependencies(
        packages, db, reverse = TRUE, which = which
    )
    revdeps <- Filter(length, revdeps)
    revdeps <- unlist(revdeps, use.names = FALSE)

    if (!length(revdeps))
        stop("No reverse dependencies on ", packages)

    listall <- biocPkgList(version = version)
    pkgMeta <- listall[listall[["Package"]] %in% revdeps, "Maintainer"]
    mainInfo <- vapply(
        unlist(pkgMeta, use.names = FALSE), .emailCut, character(1L)
    )

    mainEmails <- unname(vapply(mainInfo, .emailCut, character(1L)))

    repolinks <- .pkgsLinks(packages, version = version, md = TRUE)
    bioclinks <- .biocPkgsLinks(revdeps, version = version)

    if (nchar(PS))
        PS <- paste0("**P.S.** ", PS)

    revdeps <- paste(bioclinks, collapse = "\n\n")
    mail <- paste0(readLines(emailTemplate), collapse = "\n")
    maildate <- format(Sys.time(), "%B %d, %Y")
    firstname <- vapply(strsplit(core.name, "\\s"), `[`, character(1L), 1L)
    cpackages <- paste(packages, collapse = ", ")
    crepolinks <- paste(repolinks, collapse = ", ")
    send <- sprintf(
        mail, core.name, maildate, cpackages, crepolinks,
        revdeps, PS, firstname
    )
    title <- "Package(s) Deprecation Notification"

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

