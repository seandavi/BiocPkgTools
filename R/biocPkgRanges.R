#' Grab build report results from BUILD_STATUS_DB for a particular package range
#'
#' @param start character(1) alphabetically first package name in range
#'
#' @param end character(1) alphabetically last package name in range
#'
#' @param condition character(1) condition string, typically 'ERROR' or
#'     'WARNING'
#'
#' @param phase character(1) string for phase of event:
#'     'install', 'checksrc', or 'buildsrc' (default)
#'
#' @param version character(1) string indication Bioconductor version, either
#'     'devel' (default) or 'release'
#'
#' @author Vincent J. Carey
#'
#' @examples
#' \dontrun{
#' biocPkgRanges(
#'     start = "a4", end = "CMA",
#'     condition = "ERROR", version = "devel"
#' )
#' }
#' @export
biocPkgRanges <-
    function(start, end, condition = c("ERROR", "WARNINGS"),
        phase = "buildsrc", version = c("devel", "release"))
{
    condition <- match.arg(condition)
    version <- match.arg(version)

    sdat <- biocBuildStatusDB(version = version, pkgType = "software")

    cur <- which(sdat$package >= start & sdat$package <= end)
    if (!length(cur))
        stop("no packages in range")
    mine <- sdat[cur,]
    crit <- mine[grep("OK|skipped", mine$status, invert = TRUE), ]
    if (!nrow(crit))
        stop("all packages OK or skipped!")
    flag <- crit[grep(condition, crit$status),]
    flagged <- flag[grep(phase, flag$status),]
    split(flagged, flagged$package)
}
