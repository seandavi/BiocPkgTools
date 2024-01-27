get_build_status_db_url <- function(version, pkgType) {
  db_prefix <- if (package_version(version) >= '3.13') "BUILD_" else ""
  db_file <- paste0(db_prefix, "STATUS_DB.txt")
  sprintf(
    'https://bioconductor.org/checkResults/%s/%s-LATEST/%s',
    version, pkgType, db_file
  )
}

get_deprecated_status_df <- function(version) {
    viewsfile <- get_VIEWS(version = version, type = "BioCsoft")

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

.cache_url_file <- function(url) {
    bfc <- BiocFileCache()
    bquery <- bfcquery(bfc, url, "rname", exact = TRUE)
    needsUpdate <- bfcneedsupdate(bfc, bquery[["rid"]])
    if (identical(nrow(bquery), 1L) && (is.na(needsUpdate) || needsUpdate))
        tryCatch({
            bfcdownload(
                x = bfc, rid = bquery[["rid"]], rtype = "web", ask = FALSE
            )
        }, error = warning)

    bfcrpath(
        bfc, rnames = url, exact = TRUE, download = TRUE, rtype = "web"
    )
}
