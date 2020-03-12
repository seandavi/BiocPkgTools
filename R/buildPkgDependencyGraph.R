#' @importFrom dplyr select rename mutate filter one_of
#' @importFrom tidyr unnest
#' @importFrom tidyselect all_of
.processPkgListForDependencyGraph = function(pkglist, dependency) {
    select_list = c("Package", dependency)
    dep = dependency
    y = pkglist %>%
        select(one_of(select_list)) %>%
        rename(dependency = all_of(dependency)) %>%
        tidyr::unnest(dependency)  %>%
        mutate(dependency = stripVersionString(dependency)) %>%
        filter(!is.na(dependency)) %>%
        mutate(edgetype = dep)
    y
}

#' Work with Bioconductor package dependencies
#'
#' Bioconductor is built using an extensive set of
#' core capabilities and data structures. This leads
#' to package developers depending on other packages
#' for interoperability and functionality. This
#' function extracts package dependency information
#' from \code{\link{biocPkgList}} and returns a tidy
#' \code{data.frame} that can be used for analysis
#' and to build graph structures of package dependencies.
#'
#' @param dependencies character() vector including one or more
#' of "Depends", "Imports", or "Suggests". Default is to
#' include all possibilities.
#'
#' @param ... parameters passed along to \code{\link{biocPkgList}}
#'
#' @importFrom dplyr bind_rows
#'
#' @seealso See \code{\link{buildPkgDependencyIgraph}}, \code{\link{biocPkgList}}.
#'
#' @note This function requires network access.
#'
#' @return A \code{data.frame} (also a \code{tbl_df}) of
#' S3 class "biocDepDF" including columns "Package", "dependency",
#' and "edgetype".
#'
#' @examples
#' # performs a network call, so must be online.
#' library(BiocPkgTools)
#' depdf = buildPkgDependencyDataFrame()
#' head(depdf)
#' library(dplyr)
#' # filter to include only "Imports" type
#' # dependencies
#' imports_only = depdf %>% filter(edgetype=='Imports')
#'
#' # top ten most imported packages
#' imports_only %>% select(dependency) %>%
#'   group_by(dependency) %>% tally() %>%
#'   arrange(desc(n))
#'
#' # Bioconductor packages doing the largest
#' # amount of importing
#' largest_importers = imports_only %>%
#'   select(Package) %>%
#'   group_by(Package) %>% tally() %>%
#'   arrange(desc(n))
#'
#' # not sure what these packages do. Join
#' # to their descriptions
#' biocPkgList() %>% select(Package, Description) %>%
#'   left_join(largest_importers) %>% arrange(desc(n)) %>%
#'   head()
#' @export
buildPkgDependencyDataFrame = function(dependencies = c('Depends','Imports','Suggests'), ...) {
    dependencies <- match.arg(dependencies,
                              choices=c('Depends','Imports', 'Suggests'),
                              several.ok=TRUE)
    x = biocPkgList(...)
    df = list()
    for(i in dependencies) {
        df[[i]] = .processPkgListForDependencyGraph(x, i)
    }
    df = bind_rows(df)
    class(df) = c('biocDepDF',class(df))
    df
}

#' Work with package dependencies as a graph
#'
#' Package dependencies represent a directed
#' graph (though Bioconductor dependencies are
#' not an acyclic graph). This function simply
#' returns an igraph graph from the package
#' dependency data frame from a call to
#' \code{\link{buildPkgDependencyDataFrame}} or
#' any tidy data frame with rows of (Package, dependency)
#' pairs. Additional columns are added as igraph edge
#' attributes (see \code{\link[igraph]{graph_from_data_frame}}).
#'
#' @importFrom igraph graph_from_data_frame
#'
#' @param pkgDepDF a tidy data frame. See description for
#' details.
#'
#' @seealso See \code{\link{buildPkgDependencyDataFrame}},
#' \code{\link[igraph]{graph_from_data_frame}},
#' \code{\link{inducedSubgraphByPkgs}}, \code{\link{subgraphByDegree}},
#' \code{\link[igraph]{igraph-es-indexing}},
#' \code{\link[igraph]{igraph-vs-indexing}}
#'
#' @return An igraph directed graph. See the igraph
#' package for details of what can be done.
#'
#' @examples
#'
#' library(igraph)
#'
#' pkg_dep_df = buildPkgDependencyDataFrame()
#'
#' # at this point, filter or join to manipulate
#' # dependency data frame as you see fit.
#'
#' g = buildPkgDependencyIgraph(pkg_dep_df)
#' g
#'
#' # Look at nodes and edges
#' head(V(g)) # vertices
#' head(E(g)) # edges
#'
#' # subset graph by attributes
#'
#'
#' head(sort(degree(g, mode='in'), decreasing=TRUE))
#' head(sort(degree(g, mode='out'), decreasing=TRUE))
#'
#' @export
buildPkgDependencyIgraph = function(pkgDepDF) {
    g = igraph::graph_from_data_frame(pkgDepDF)
    class(g) = c('biocDepGraph',class(g))
    g
}

#' Return a minimal subgraph based on package name(s)
#'
#' Find the subgraph induced by including
#' specific packages. The induced subgraph is
#' the graph that includes the named packages
#' and all edges connecting them. This is useful
#' for a developer, for example, to examine her packages
#' and their intervening dependencies.
#'
#' @param g an igraph graph, typically created by
#' \code{\link{buildPkgDependencyIgraph}}
#'
#' @param pkgs character() vector of packages to
#' include. Package names not included in
#' the graph are ignored.
#'
#' @param pkg_color character(1) giving color of named
#' packages. Other packages in the graph that fall in
#' connecting paths will be colored as the igraph default.
#'
#' @importFrom igraph induced_subgraph V
#'
#' @examples
#' library(igraph)
#' g = buildPkgDependencyIgraph(buildPkgDependencyDataFrame())
#' g2 = inducedSubgraphByPkgs(g, pkgs=c('GenomicFeatures',
#' 'TCGAbiolinksGUI', 'BiocGenerics', 'org.Hs.eg.db', 'minfi', 'limma'))
#' g2
#' V(g2)
#'
#' plot(g2)
#'
#' @export
inducedSubgraphByPkgs = function(g, pkgs, pkg_color='red') {
    pkgs = intersect(pkgs, names(igraph::V(g)))
    g2 = igraph::induced_subgraph(graph=g, vids=pkgs)
    igraph::V(g2)[pkgs]$color='red'
    g2
}

#' Subset graph by degree
#'
#' While the \code{\link{inducedSubgraphByPkgs}}
#' returns the subgraph with the minimal connections
#' between named packages, this function takes a vector of
#' package names, a degree (1 or more) and returns the
#' subgraph(s) that are within \code{degree} of the
#' package named.
#'
#' @param g an igraph graph, typically created by
#' \code{\link{buildPkgDependencyIgraph}}
#'
#' @param pkg character(1) package name from which to
#' measure degree.
#'
#' @param degree integer(1) degree, limit search for
#' adjacent vertices to this degree.
#'
#' @param ... passed on to \code{\link[igraph]{distances}}
#'
#' @importFrom igraph distances induced_subgraph V is.igraph
#'
#' @return An igraph graph, with only nodes and their
#' edges within degree of the named package
#'
#' @examples
#'
#' g = buildPkgDependencyIgraph(buildPkgDependencyDataFrame())
#' g2 = subgraphByDegree(g, 'GEOquery')
#' g2
#'
#' @export
subgraphByDegree = function(g, pkg, degree=1, ...) {
    stopifnot(is.character(pkg) & pkg %in% names(igraph::V(g)) & length(pkg)==1)
    stopifnot(is.igraph(g))
    d = igraph::distances(graph=g, v=pkg, ...)
    d2 = d[1,d[1,]<= degree]
    igraph::induced_subgraph(graph=g, vids=names(d2))
}
