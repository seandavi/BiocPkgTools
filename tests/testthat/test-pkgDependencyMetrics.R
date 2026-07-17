## --- internal helpers: no network needed -----------------------------------

test_that(".flat_map_lst maps and flattens", {
    expect_null(BiocPkgTools:::.flat_map_lst(list(), function(x) x))
    expect_identical(
        BiocPkgTools:::.flat_map_lst(list(1, 2, 3), function(x) x * 2),
        c(2, 4, 6)
    )
})

test_that(".char_or_sym extracts characters and symbols", {
    expect_identical(BiocPkgTools:::.char_or_sym("abc"), "abc")
    expect_identical(BiocPkgTools:::.char_or_sym(quote(foo)), "foo")
    ## anything else (e.g. a number) yields character(0)
    expect_identical(BiocPkgTools:::.char_or_sym(1L), character(0))
})

test_that(".dep_usage_lang extracts pkg::fun and bare calls", {
    res_ns <- BiocPkgTools:::.dep_usage_lang(quote(pkg::fun(a, b)))
    expect_s3_class(res_ns, "data.frame")
    expect_identical(res_ns$pkg, "pkg")
    expect_identical(res_ns$fun, "fun")

    res_bare <- BiocPkgTools:::.dep_usage_lang(quote(fun(a, b)))
    expect_s3_class(res_bare, "data.frame")
    ## a call with no explicit namespace records the literal string "NA"
    ## (not NA_character_) for 'pkg' -- this quirk is relied upon downstream
    ## by pkgDepImports(), which explicitly recodes the string "NA" to
    ## NA_character_ (see R/pkgDependencyMetrics.R).
    expect_identical(res_bare$pkg[1], "NA")
    expect_identical(res_bare$fun[1], "fun")

    ## a literal expression with no calls returns NULL (invisibly, via f())
    res_none <- BiocPkgTools:::.dep_usage_lang(quote(1))
    expect_null(res_none)
})

test_that(".pkgDepCheckArgs validates depdf shape and package presence", {
    good_depdf <- data.frame(
        Package = "Foo", dependency = "Bar", edgetype = "Imports",
        stringsAsFactors = FALSE
    )
    expect_error(
        BiocPkgTools:::.pkgDepCheckArgs("Foo", data.frame(x = 1)),
        "must be a 'data.frame' with columns"
    )
    expect_error(
        BiocPkgTools:::.pkgDepCheckArgs("NotThere", good_depdf),
        "not in the package dependency"
    )
    expect_null(BiocPkgTools:::.pkgDepCheckArgs("Foo", good_depdf))
})

test_that(".getDepGain computes dependency gain on a synthetic graph", {
    ## A -> B -> D
    ## A -> C -> D
    edges <- data.frame(
        from = c("A", "A", "B", "C"), to = c("B", "C", "D", "D"),
        stringsAsFactors = FALSE
    )
    g <- igraph::graph_from_data_frame(edges)

    ## removing the A->B edge: D is still reachable via C, so only B is lost
    expect_equal(BiocPkgTools:::.getDepGain(g, "A", "B"), 1)

    ## removing both direct edges: B, C and D all become unreachable
    expect_equal(BiocPkgTools:::.getDepGain(g, "A", c("B", "C")), 3)
})

## --- pkgDepImports: operates on an installed package's NAMESPACE, no network ---

test_that("pkgDepImports reports functionality calls used from dependencies", {
    res <- pkgDepImports("BiocPkgTools")
    expect_s3_class(res, "tbl_df")
    expect_true(all(c("pkg", "fun") %in% colnames(res)))
    expect_gt(nrow(res), 0L)
    ## imports from this package's own namespace should be excluded
    expect_false("BiocPkgTools" %in% res$pkg)
    ## base R functionality is filtered out
    expect_false("base" %in% res$pkg)
})

test_that("pkgDepImports errors for a package that isn't loadable", {
    expect_error(pkgDepImports("not-a-real-package-xyz"))
})

## --- pkgCombDependencyGain / pkgDepMetrics: need a real dependency graph ---

skip_if_bioc_offline()

depdf <- buildPkgDependencyDataFrame(
    dependencies = c("Depends", "Imports"),
    repo = c("BioCsoft", "CRAN")
)

test_that("pkgCombDependencyGain reports dependency gain per combination", {
    skip_if_not(
        "GEOquery" %in% depdf$Package, "GEOquery not in dependency data frame"
    )
    res <- pkgCombDependencyGain("GEOquery", depdf, maxNbr = 2L)

    expect_s3_class(res, "data.frame")
    expect_true(all(c("Packages", "NbrExcl", "DepGain") %in% colnames(res)))
    expect_gt(nrow(res), 0L)
    expect_true(all(res$NbrExcl %in% c(1L, 2L)))
    expect_true(all(res$DepGain >= 0))
})

test_that("pkgCombDependencyGain errors for an unknown package", {
    expect_error(
        pkgCombDependencyGain("not-a-real-package-xyz", depdf),
        "not in the package dependency"
    )
})

test_that("pkgDepMetrics reports usage metrics ordered by Usage", {
    res <- pkgDepMetrics("BiocPkgTools", depdf)

    expect_s3_class(res, "data.frame")
    expect_true(all(
        c("ImportedAndUsed", "Exported", "Usage", "DepOverlap",
          "DepGainIfExcluded") %in% colnames(res)
    ))
    expect_gt(nrow(res), 0L)
    ## results are ordered by ascending Usage (NAs sort last)
    usage <- res$Usage
    expect_identical(order(usage, na.last = TRUE), seq_along(usage))
})

test_that("pkgDepMetrics errors for an unknown package", {
    expect_error(
        pkgDepMetrics("not-a-real-package-xyz", depdf),
        "not in the package dependency"
    )
})
