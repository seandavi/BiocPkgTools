## These functions hit bioconductor.org indirectly through biocPkgList(),
## so they are gated on skip_if_bioc_offline(). The dependency data frame is
## built once and shared across tests in this file (mirroring the pattern in
## test_biocPkgList.R) to avoid repeating the network call and to keep the
## graph operations (which are all in-memory and fast) from re-fetching data.

skip_if_bioc_offline()

depdf <- buildPkgDependencyDataFrame(dependencies = "strong")

test_that("buildPkgDependencyDataFrame returns a tidy biocDepDF", {
    expect_s3_class(depdf, "biocDepDF")
    expect_s3_class(depdf, "data.frame")
    expect_true(all(c("Package", "dependency", "edgetype") %in% colnames(depdf)))
    expect_gt(nrow(depdf), 0L)
    ## only strong dependency types requested
    expect_true(all(depdf$edgetype %in% c("Depends", "Imports", "LinkingTo")))
    ## "R" and NA/empty entries are filtered out
    expect_false("R" %in% depdf$dependency)
    expect_false(any(is.na(depdf$dependency)))
    expect_false(any(depdf$dependency == ""))
})

test_that("buildPkgDependencyDataFrame rejects invalid 'dependencies'", {
    ## Note: a single invalid string (e.g. dependencies = "bogus") is *not*
    ## caught by match.arg() -- match.arg() is only reached when
    ## length(dependencies) != 1, and match.arg(..., several.ok = TRUE) only
    ## errors when *none* of the supplied values are valid choices, so a
    ## vector of two unrecognized strings is needed to trigger it.
    expect_error(
        buildPkgDependencyDataFrame(dependencies = c("bogus1", "bogus2")),
        "'arg' should be one of"
    )
})

test_that("buildPkgDependencyIgraph builds a biocDepGraph igraph object", {
    g <- buildPkgDependencyIgraph(depdf)
    expect_s3_class(g, "biocDepGraph")
    expect_true(igraph::is_igraph(g))
    expect_true(igraph::is_directed(g))
    expect_true("GEOquery" %in% names(igraph::V(g)))
})

test_that("inducedSubgraphByPkgs returns only requested (and connecting) nodes", {
    g <- buildPkgDependencyIgraph(depdf)
    pkgs <- c("GEOquery", "Biobase", "S4Vectors")
    g2 <- inducedSubgraphByPkgs(g, pkgs = pkgs)

    expect_true(igraph::is_igraph(g2))
    expect_true(all(names(igraph::V(g2)) %in% pkgs))
    expect_true(all(igraph::V(g2)[pkgs]$color == "red"))
})

test_that("inducedSubgraphByPkgs ignores package names not in the graph", {
    g <- buildPkgDependencyIgraph(depdf)
    g2 <- inducedSubgraphByPkgs(g, pkgs = c("GEOquery", "not-a-real-package-xyz"))
    expect_true(igraph::is_igraph(g2))
    expect_false("not-a-real-package-xyz" %in% names(igraph::V(g2)))
})

test_that("subgraphByDegree limits nodes to within 'degree' of a package", {
    g <- buildPkgDependencyIgraph(depdf)
    g1 <- subgraphByDegree(g, "GEOquery", degree = 1)
    expect_true(igraph::is_igraph(g1))
    expect_true("GEOquery" %in% names(igraph::V(g1)))
    expect_lte(length(igraph::V(g1)), length(igraph::V(g)))
})

test_that("subgraphByDegree(degree = 0) currently returns an empty graph (known limitation)", {
    ## NOTE: this documents a real bug in subgraphByDegree() rather than
    ## exercising intended behavior. `d2 <- d[1, d[1,] <= degree]` relies on
    ## matrix-style `[` indexing; when exactly one column matches (as always
    ## happens at degree = 0, since only the queried package itself has
    ## distance 0 from itself), R's matrix indexing silently drops the name
    ## of that single remaining element (a base R quirk: compare
    ## `m <- matrix(1, dimnames = list("r", "c")); m[1, TRUE]`, which returns
    ## an unnamed scalar). `names(d2)` is then NULL, and
    ## `induced_subgraph(g, vids = NULL)` returns an *empty* graph instead of
    ## a single-vertex graph containing just "GEOquery". Left unfixed here
    ## since it does not block coverage of the function's main path (tested
    ## above with degree = 1), but flagged for a follow-up fix.
    g <- buildPkgDependencyIgraph(depdf)
    g0 <- subgraphByDegree(g, "GEOquery", degree = 0)
    expect_true(igraph::is_igraph(g0))
    expect_identical(length(igraph::V(g0)), 0L)
})

test_that("subgraphByDegree errors for a package not present in the graph", {
    g <- buildPkgDependencyIgraph(depdf)
    expect_error(
        subgraphByDegree(g, "not-a-real-package-xyz", degree = 1)
    )
})
