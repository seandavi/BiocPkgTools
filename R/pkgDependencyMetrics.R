## private function from itdepends/R/utils.R
.flat_map_lst <- function(x, f, ...) {
  if (length(x) == 0) {
    NULL
  } else {
    if (typeof(x) == "closure" && !inherits(x, "function")) {
      class(x) <- c(class(x), "function")
    }
    unlist(lapply(x, f, ...), recursive = FALSE, use.names = FALSE)
  }
}

## private function from itdepends/R/utils.R
.char_or_sym <- function(x) {
  if (is.character(x)) {
    x
  } else if (is.symbol(x)) {
    as.character(x)
  } else {
    character()
  }
}

## Private function from itdepends/R/dep_locate.R
#' @importFrom rlang is_syntactic_literal is_symbol is_pairlist is_call
.dep_usage_lang <- function(x) {
  f <- function(x) {
    ## currently, for constants such as Biostrings::DNA_BASES
    ## is_symbol(x) == TRUE and therefore won't reported as
    ## functionality calls
    if (is_syntactic_literal(x) || is_symbol(x)) {
      return(NULL)
    }

    if (is_pairlist(x) || is.expression(x)) {
      return(.flat_map_lst(x, f))
    }

    if (is_call(x, c("::", ":::"))) {
      return(list(pkg = .char_or_sym(x[[2]]), fun = .char_or_sym(x[[3]])))
    }

    if (is_call(x) && length(x[[1]]) == 1) {
      return(
        c(
          list(pkg = NA, fun = .char_or_sym(x[[1]])),
          .flat_map_lst(x, f)
          )
        )
    }

    .flat_map_lst(x, f)
  }

  res <- f(x)
  if (length(res) > 0) {
    data.frame(
      pkg = as.character(res[seq(1, length(res), 2)]),
      fun = as.character(res[seq(2, length(res), 2)]), stringsAsFactors = FALSE)
  }
}

#' Report package imported functionality
#'
#' Function adapted from 'itdepends::dep_usage_pkg' at https://github.com/r-lib/itdepends
#'
#' @importFrom tibble tibble
#' @importFrom stats setNames
#'
#' @param pkg character() name of the packge for which we want
#' to obtain metrics on its dependency burden.
#'
#' @return a tidy data frame with different metrics on the
#' package dependency burden.
#'
#' @export
pkgDepImports <- function(pkg) {

  ## fetch all imported functionality
  imp <- getNamespaceImports(pkg)
  mask <- sapply(imp, isTRUE)
  if (any(mask)) {
    imp[mask] <- lapply(names(imp)[mask], function(pkg2) {
      ns <- getNamespace(pkg2)
      exports <- getNamespaceExports(ns)
      nms <- intersect(exports, ls(envir=ns, all.names=TRUE, sorted=FALSE))
      setNames(nms, nms)
    })
  }
  imp <- lapply(imp, grep, pattern="^.__|^-.", invert=TRUE, value=TRUE)

  ## 'getNamespaceImports()' returns a list with imported functionality
  ## by package but the imported functionality of a package may be scattered
  ## throughout more than one list entry with the same name. here we glue
  ## together those entries to have a single one per package
  fun_to_imp <- setNames(rep(names(imp), lengths(imp)),
                         unlist(imp, use.names=FALSE))
  imp <- split(names(fun_to_imp), fun_to_imp)

  ## not all imported functionality may be actually used in a package,
  ## e.g., 'import(IRanges)' and using 'findOverlaps()' only. here we
  ## fetch all calls made from the package to know what is actually
  ## being used.
  pkg_funs <- mget(ls(envir=asNamespace(pkg), all.names=TRUE,
                      sorted=FALSE),
                   envir=asNamespace(pkg), mode="function",
                   inherits=TRUE, ifnotfound=NA)

  ## according to https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#S4-objects
  ## names starting with .__C__classname correspond to classes and .__T__generic:package
  ## correspond to methods. for these two entries, 'pkg_funs' is NA and we should
  ## treat them separately
  mask <- sapply(pkg_funs, function(x) is_syntactic_literal(x) && is.na(x))
  pkg_calls <- do.call(rbind, c(lapply(pkg_funs[!mask], .dep_usage_lang),
                                lapply(strsplit(sub("^\\.__.__", "",
                                                    names(pkg_funs[mask])), ":"),
                                       function(x) c(pkg=x[2], fun=x[1])),
                                make.row.names=FALSE,
                                stringsAsFactors=FALSE))

  ## remove calls to functionality from 'pkg' itself. a call to a functionality is
  ## from the 'pkg' itself ("ours") if either is annotated to 'pkg' OR is not imported
  ## AND either is not annotated either to 'pkg' or has a missing package annotation
  pkg_calls$pkg[pkg_calls$pkg == "NA"] <- NA_character_
  missing_pkg <- is.na(pkg_calls$pkg)
  our_pkg <- !is.na(pkg_calls$pkg) & pkg_calls$pkg == pkg
  ours <- our_pkg | ((!pkg_calls$fun %in% names(fun_to_imp)) & (!our_pkg | missing_pkg))
  pkg_calls <- pkg_calls[!ours, ]
  pkg_calls$pkg[is.na(pkg_calls$pkg)] <- "__otherpkgs__"

  ## group calls to functionality by imported package
  pkg_calls <- split(pkg_calls$fun, pkg_calls$pkg)

  ## now filter out from the imported functionality the part
  ## that is not actually being called from the package
  imp <- lapply(as.list(setNames(names(imp), names(imp))),
                function(nm, implst, pkgcallslst) {
                  res <- intersect(implst[[nm]], pkgcallslst[["__otherpkgs__"]])
                  if (nm %in% names(pkgcallslst))
                    res <- intersect(implst[[nm]], pkgcallslst[[nm]])
                  res
                  }, imp, pkg_calls)
  ## remove functionality from 'base' R
  imp$base <- NULL

  res <- tibble(pkg=rep(names(imp), lengths(imp)),
                fun=unlist(imp, use.names=FALSE))
  res
}


#' Report package dependency burden
#'
#' Elaborate a report on the dependency burden of a given package.
#'
#' @importFrom igraph subcomponent ego
#'
#' @param pkg character() name of the packge for which we want
#' to obtain metrics on its dependency burden.
#'
#' @param depdf a tidy data frame with package dependency information
#' obtained through the function \code{\link{buildPkgDependencyDataFrame}}
#'
#' @return a tidy data frame with different metrics on the
#' package dependency burden.
#'
#' @export
pkgDepMetrics <- function(pkg, depdf) {

  ## fetch dependency graph
  g <- buildPkgDependencyIgraph(depdf)

  ## exclude 'R', 'base' and 'methods'
  excludedpkgs <- c("R", "base", "methods")
  g <- induced_subgraph(g, setdiff(names(V(g)), excludedpkgs))

  ## get all reachable dependencies
  deppkgs <- subcomponent(g, pkg, mode="out")

  ## get the induced subgraph of dependencies for 'pkg'
  g.pkg <- induced_subgraph(g, deppkgs)

  ## fetch first level dependencies
  dep1pkgs <- names(ego(g.pkg, nodes=pkg, mode="out", mindist=1)[[1]])

  ## fetch imported functionality
  du <- pkgDepImports(pkg)
  du <- sapply(split(du$fun, du$pkg), unique)
  du <- lengths(du)

  ifun <- efun <- integer(length(dep1pkgs))
  names(ifun) <- names(efun) <- dep1pkgs
  ifun[dep1pkgs] <- du[dep1pkgs]

  depov <- numeric(length(dep1pkgs))
  names(depov) <- dep1pkgs

  gvtx <- names(V(g.pkg))
  for (p in dep1pkgs) {
    deppkgs <- subcomponent(g, p, mode="out")
    gdep <- induced_subgraph(g, deppkgs)
    gdepvtx <- names(V(gdep))
    depov[p] <- length(intersect(gvtx, gdepvtx)) / length(union(gvtx, gdepvtx))
    expfun <- getNamespaceExports(p)
    efun[p] <- length(expfun[grep("^\\.", expfun, invert=TRUE)])
  }

  res <- data.frame(ImportedAndUsed=ifun,
                    Exported=efun,
                    Usage=round(100*ifun/efun, digits=2),
                    DepOverlap=round(depov, digits=2),
                    row.names=dep1pkgs,
                    stringsAsFactors=FALSE)
  res[order(res$Usage), ]
}
