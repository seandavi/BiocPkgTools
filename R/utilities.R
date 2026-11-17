get_build_status_db_url <- function(version, pkgType) {
  db_prefix <- if (package_version(version) >= '3.13') "BUILD_" else ""
  db_file <- paste0(db_prefix, "STATUS_DB.txt")
  sprintf(
    'https://bioconductor.org/checkResults/%s/%s-LATEST/%s',
    version, pkgType, db_file
  )
}

.get_build_report_tgz_url <- function(version, pkgType) {
    url <- .get_build_report_url(version = version, pkgType = pkgType)
    paste0(url, "/report.tgz")
}

.get_build_report_url <- function(version, pkgType) {
    sprintf(
        'https://bioconductor.org/checkResults/%s/%s-LATEST',
        version, pkgType
    )
}

get_deprecated_status_df <- function(version) {
    viewsfile <- .get_VIEWS(version = version, type = "BioCsoft")

    if (nrow(viewsfile))
        depdf <- cbind.data.frame(
            Package = viewsfile[, "Package"],
            Deprecated = viewsfile[, "PackageStatus"] == "Deprecated" &
                !is.na(viewsfile[, "PackageStatus"]),
            PackageStatus = viewsfile[, "PackageStatus"]
        )
    else
        depdf <- data.frame(
            Package = character(0L),
            Deprecated = logical(0L),
            PackageStatus = character(0L)
        )

    depdf
}

.read_summary_dcfs <- function(dcf_location) {
    summary_dcfs <- list.files(dcf_location, pattern="-summary\\.dcf$",
        full.names = TRUE, recursive = TRUE)
    fields <- c("Package", "StartedAt", "EndedAt", "EllapsedTime")
    summaries <- lapply(summary_dcfs, .import_dcf_stage_node, fields = fields)
    as.data.frame(do.call(rbind, summaries))
}

.import_dcf_stage_node <- function(filepath, fields) {
    stage <- head(strsplit(basename(filepath), "-", fixed = TRUE)[[1L]], 1L)
    node <- basename(dirname(filepath))
    dcf_pkg <- read.dcf(filepath, fields = fields)
    dcf_chr <- structure(as.character(dcf_pkg), .Names = fields)
    append(dcf_chr, c(node = node, stage = stage), after = 1L)
}

.read_info_dcfs <- function(info_files_location) {
    dcffiles <- list.files(path = info_files_location,
        pattern = "info\\.dcf$", full.names = TRUE, recursive = TRUE)
    meta <- do.call(rbind.data.frame, lapply(dcffiles, read.dcf))
    y <- meta[,
        c("Package", "Maintainer", "Version",
        "git_last_commit", "git_last_commit_date")
    ]
    y[["git_last_commit_date"]] <-
        as.POSIXct(gsub("^(.*)\\s\\(.*", "\\1", y[["git_last_commit_date"]]))
    names(y)[1:3] <- c("pkg", "author", "version")
    y
}

## HEAD the url and return its size and cache validators, or NULL if the
## request fails. 'Accept-Encoding: identity' keeps servers that compress
## on the fly (e.g. Cloudflare) from reporting a compressed size or a weak
## ETag.
.get_remote_info <- function(url) {
    tryCatch({
        response <- httr2::request(url) |>
            httr2::req_method("HEAD") |>
            httr2::req_headers("Accept-Encoding" = "identity") |>
            httr2::req_error(is_error = function(resp) { FALSE }) |>
            httr2::req_perform()
        if (httr2::resp_is_error(response))
            return(NULL)
        header <- function(name) {
            value <- httr2::resp_header(response, name)
            if (is.null(value)) NA_character_ else value
        }
        list(
            size = as.numeric(header("content-length")),
            etag = header("etag"),
            modified = header("last-modified")
        )
    }, error = function(e) {
        NULL
    })
}

.get_remote_size <- function(url) {
    info <- .get_remote_info(url)
    if (is.null(info))
        return(NA_integer_)
    info[["size"]]
}

## Strip quotes and the weak-validator prefix so that the strong and weak
## forms of the same ETag compare equal.
.normalize_etag <- function(etag) {
    gsub('^W/|"', "", etag)
}

## Parse an RFC 7231 HTTP date ("Mon, 16 Nov 2026 09:52:30 GMT");
## day and month names are English regardless of locale.
.parse_http_date <- function(x) {
    old <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
    Sys.setlocale("LC_TIME", "C")
    as.POSIXct(strptime(x, "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
}

## TRUE if the cached copy of 'url' is stale, FALSE if current, NA if 'url'
## is not cached (or can't be checked).
##
## Staleness is judged from the remote size, ETag and Last-Modified rather
## than BiocFileCache::bfcneedsupdate(), which compares the 'expires' date
## at day granularity: with a short 'Cache-Control: max-age' (as
## bioconductor.org now sends) every resource is 'expired' as soon as it
## is downloaded, forcing a fresh download on every call (#92).
.needs_update <- function(bfc, bquery, url) {
    if (!identical(nrow(bquery), 1L))
        return(NA)

    remote <- .get_remote_info(url)
    if (is.null(remote))
        return(bfcneedsupdate(bfc, bquery[["rid"]]))

    local_path <- bfcrpath(bfc, rids = bquery[["rid"]])
    if (!file.exists(local_path))
        return(TRUE)
    if (!is.na(remote[["size"]]) && file.size(local_path) != remote[["size"]])
        return(TRUE)

    local_etag <- bquery[["etag"]]
    if (!is.na(local_etag) && !is.na(remote[["etag"]]))
        return(
            !identical(
                .normalize_etag(unname(local_etag)),
                .normalize_etag(remote[["etag"]])
            )
        )

    local_time <- as.POSIXct(
        bquery[["last_modified_time"]], tz = "GMT", optional = TRUE
    )
    remote_time <- .parse_http_date(remote[["modified"]])
    if (!is.na(local_time) && !is.na(remote_time))
        return(remote_time > local_time)

    FALSE
}

.cache_url_file <- function(url) {
    bfc <- BiocFileCache()
    bquery <- bfcquery(bfc, url, "rname", exact = TRUE)
    needsUpdate <- .needs_update(bfc, bquery, url)

    if (
        identical(nrow(bquery), 1L) && (is.na(needsUpdate) || needsUpdate)
    )
        tryCatch(
            bfcdownload(x = bfc, rid = bquery[["rid"]], ask = FALSE),
            error = function(e) warning(
                "could not refresh ", url, "; using cached copy: ",
                conditionMessage(e), call. = FALSE
            )
        )

    bfcrpath(
        bfc, rnames = url, exact = TRUE, download = TRUE, rtype = "web"
    )
}

.url_exists <- function(url) {
    response <- httr2::request(url) |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    status <- !httr2::resp_is_error(response)
    if (!status)
        warning(url, " cannot be reached", call. = FALSE)
    status
}
