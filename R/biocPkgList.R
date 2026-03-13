#' Extract funder names from an \code{Authors@R} field value
#'
#' @param authors_at_r_string character(1) the value of the \code{Authors@R}
#'   DESCRIPTION field
#'
#' @return character() of funder names, or \code{NA_character_} when no funder
#'   is found or parsing fails
#'
#' @keywords internal
.extract_fnd <- function(authors_at_r_string) {
    if (is.na(authors_at_r_string) || !nzchar(trimws(authors_at_r_string)))
        return(NA_character_)
    tryCatch({
        authors <- eval(parse(text = authors_at_r_string))
        fnd_persons <- Filter(function(p) "fnd" %in% p$role, authors)
        if (!length(fnd_persons))
            return(NA_character_)
        vapply(fnd_persons, function(p) {
            name_parts <- c(p$given, p$family)
            paste(name_parts[!is.na(name_parts) & nzchar(name_parts)],
                  collapse = " ")
        }, character(1))
    }, error = function(e) NA_character_)
}

#' @import biocViews
#' @importFrom RBGL transitive.closure
.computeBiocViewsTransitiveClosure <- function() {
  data("biocViewsVocab", package = "biocViews", envir = environment())
  return(RBGL::transitive.closure(biocViewsVocab))
}

#' @importFrom graph edges nodes
.biocChildToParent <- function() {
  biocViewsTC = .computeBiocViewsTransitiveClosure()
  biocViewsTCEdges = edges(biocViewsTC)
  df = data.frame(child = unlist(biocViewsTCEdges, use.names = FALSE),
                  parent = rep(names(biocViewsTCEdges), sapply(biocViewsTCEdges, length)),
                  stringsAsFactors = FALSE)
  # Remove BiocViews, as this is simply the root
  df = df[df$parent!='BiocViews',]
  return(split(df$parent, df$child))
}

#' Get full Bioconductor software package listing, with details
#'
#' The BiocViews-generated `VIEWS` file is available
#' for Bioconductor release and devel repositories. It
#' contains quite a bit more information from the
#' package `DESCRIPTION` files than the `PACKAGES`
#' file. In particular, it contains `biocViews` annotations
#' and URLs for vignettes and developer URLs.
#'
#' Since packages are annotated with the most specific
#' views, the default functionality here is to add parent terms
#' for all views for each package. For example, in the bioCsoft
#' repository, all packages will have at least "Software" added
#' to their biocViews. If one wants to stick to only the most
#' specific terms, set `addBiocViewParents` to `FALSE`.
#'
#' @inheritParams biocBuildReport
#'
#' @param repo `character(1)` The requested Bioconductor repository. The default
#'   is to pull from the "BioCsoft" repository. Possible repositories include
#'   "BioCsoft", "BioCexp", "BioCworkflows", "BioCann", and "CRAN". Note that
#'   not all repos are available for all versions, particularly older versions.
#'
#' @param addBiocViewParents `logical(1)` whether to add all biocViews
#'    parents to biocViews annotations.
#'
#' @return An object of class `tbl_df`. When the `Authors@R` field is present
#'   in the VIEWS data, the result includes a `fnd` list-column whose elements
#'   are character vectors of funder names extracted from persons with role
#'   `"fnd"`. Elements are `NA_character_` for packages that declare no
#'   funder.
#'
#' @importFrom BiocManager repositories version
#' @importFrom stringr str_split str_replace_all str_remove_all str_squish
#' @importFrom tibble as_tibble
#'
#' @examples
#' bpkgl <- biocPkgList(repo = "BioCsoft")
#' bpkgl
#' unlist(bpkgl[1,'Depends'], use.names = FALSE)
#'
#' # Get a list of all packages that
#' # import "GEOquery"
#' library(dplyr)
#' bpkgl |>
#'   filter(Package == 'GEOquery') |>
#'   pull('importsMe') |>
#'   unlist()
#'
#' @export
biocPkgList <- function(
    version = BiocManager::version(),
    repo = c("BioCsoft", "BioCexp", "BioCworkflows", "BioCann", "CRAN"),
    addBiocViewParents = TRUE
) {
    if(!is.logical(addBiocViewParents) ||
       !(addBiocViewParents %in% c(TRUE,FALSE)))
      stop('addBiocViewParents must be a single logical.')
    if (missing(repo))
        repo <- "BioCsoft"
    else
        repo <- match.arg(repo, several.ok = TRUE)
    if (!is.character(repo))
      stop('repo must be a character vector.')

    if (version %in% c("release", "devel"))
        version <- BiocManager:::.version_bioc(version)

    version <- try(as.package_version(version), silent = TRUE)

    if (is(version, "try-error"))
        stop(
            "'version' is not 'release', 'devel', or a valid 'package_version'"
        )
    repos <- BiocManager:::.repositories(
        site_repository = character(), version = version
    )
    mask <- !repo %in% names(repos)
    if (any(mask))
      warning(sprintf("repo %s not found in known repositories. Valid values for repo can be shown by running names(BiocManager::repositories()).",
                      paste(repo[mask], collapse=", ")))
    if (all(mask))
      stop("No repo was found in known repositories.")
    repo <- repo[!mask]

    ret <- lapply(as.list(repo),
                  function(r) {
                    if (r != "CRAN") {
                      ret <- .get_VIEWS(version = version, type = r)
                    } else {
                      ret <- get_CRAN_pkg_rds()
                      ## to increase the overlap of the information from
                      ## CRAN with Bioconductor, replace CRAN column names
                      ## to Bioconductor column names for 'reverse imports',
                      ## 'depends' and 'suggests'
                      biocrevnames <- c(importsMe="Reverse imports",
                                        dependsOnMe="Reverse depends",
                                        suggestsMe="Reverse suggests")
                      mt <- match(biocrevnames, colnames(ret))
                      mask <- !is.na(mt)
                      colnames(ret)[mt[mask]] <- names(biocrevnames)[mask]
                      ret$biocViews<- NA_character_
                    }

                    # convert comma-delimited text columns into
                    # list columns
                    commaCols = c('Depends', 'Suggests', 'dependsOnMe', 'Imports', 'importsMe',
                                'Enhances', 'vignettes', 'vignetteTitles', 'suggestsMe',
                                'Maintainer', 'biocViews', 'Archs', 'linksToMe', 'LinkingTo',
                                'Rfiles')
                    commaCols = intersect(commaCols, colnames(ret))
                    for(commaCol in commaCols) {
                        ret[[commaCol]] = str_split(ret[[commaCol]],'\\s?,\\s?')
                    }
                    if(r != "CRAN" && addBiocViewParents) {
                      child2parentMap = .biocChildToParent()
                      tmp = lapply(ret$biocViews, function(views) {
                        return(unique(c(unlist(child2parentMap[views]),views)))
                      })
                      ret$biocViews = tmp
                    }

                    ret[["Author"]] = ret[["Author"]] |>
                        str_replace_all("\n", " ") |>
                        str_remove_all("\\[.*?\\]") |>
                        str_remove_all("<.*?>") |>
                        str_remove_all("\\(.*?\\)") |>
                        str_squish() |>
                        str_replace_all("\\w* contributions ?\\w*", ", ") |>
                        str_replace_all("\\sand\\s", ", ") |>
                        str_replace_all(",\\s+,", ",") |>
                        str_replace_all(",+", ",")

                    ret[["Author"]] = lapply(
                        str_split(ret[["Author"]], ","),
                        str_squish
                    )

                    ## Extract funder names from Authors@R if available
                    if ("Authors@R" %in% colnames(ret)) {
                        ret[["fnd"]] <- lapply(ret[["Authors@R"]],
                                               .extract_fnd)
                    } else {
                        ret[["fnd"]] <- as.list(
                            rep(NA_character_, nrow(ret))
                        )
                    }
                    ret
                  })

    ## build a table with the common columns provided by the different
    ## repositories. if no common columns exist, then prompt an error
    commonCols <- Reduce(intersect, lapply(ret, colnames))
    if (length(commonCols) == 0)
      stop(sprintf("No common columns between repos in %s",
                   paste(repo, collapse=", ")))

    ret <- lapply(ret, function(x, cnames) x[, cnames], commonCols)
    ret <- do.call("rbind", ret)

    ret = as_tibble(ret)
    class(ret) = c("biocPkgList", class(ret))
    ret
}

stripVersionString <- function(s) {
    sub('\\s?\\(.*\\)\\s?','',s)
}

CRAN_pkg_rds_url <- function() {
    paste0(.CRAN_WEB_URL, "/web/packages/packages.rds")
}

read_CRAN_url <- function(url) {
    con <- url(url)
    on.exit(close(con))
    res <- try(readRDS(gzcon(con)), silent = TRUE)
    if (inherits(res, "try-error"))
        stop("'packages.rds' not found in CRAN 'repo'", call. = FALSE)
    else
        as.data.frame(res, stringsAsFactors = FALSE)
}

get_CRAN_pkg_rds <- function() {
    cran_url <- CRAN_pkg_rds_url()
    read_CRAN_url(cran_url)
}
