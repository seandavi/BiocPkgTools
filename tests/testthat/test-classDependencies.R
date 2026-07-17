## These functions operate purely on installed packages' NAMESPACE / S4 class
## metadata -- no network access is required, so nothing here is gated on
## skip_if_offline()/skip_if_bioc_offline(). SummarizedExperiment and Biobase
## (both Bioconductor packages with rich S4 class hierarchies) are used as
## small, already-installed targets.

suppressPackageStartupMessages(library(SummarizedExperiment))
suppressPackageStartupMessages(library(Biobase))

## --- internal helpers -------------------------------------------------------

test_that(".is_s_virtual identifies virtual classes", {
    ## 'Vector' is virtual, 'DFrame' is a concrete (non-virtual) S4Vectors class
    res <- BiocPkgTools:::.is_s_virtual(c("Vector", "DFrame"))
    expect_identical(res, c(TRUE, FALSE))
    ## NA input short-circuits to FALSE
    expect_identical(BiocPkgTools:::.is_s_virtual(NA_character_), FALSE)
})

test_that(".is_s_union identifies S4 class unions", {
    ## 'vector_OR_Vector' is a real class union exported by S4Vectors
    expect_true(BiocPkgTools:::.is_s_union("vector_OR_Vector"))
    expect_false(BiocPkgTools:::.is_s_union("DataFrame"))
})

test_that(".get_s_extends_data.frame builds a well-formed edge data.frame", {
    df <- BiocPkgTools:::.get_s_extends_data.frame(
        parent = "P", child = "C", type = "object"
    )
    expect_s3_class(df, "data.frame")
    expect_identical(df$parent, "P")
    expect_identical(df$child, "C")
    expect_identical(
        colnames(df),
        c("parent", "child", "type", "parentVirtual", "parentUnion",
          "childVirtual", "childUnion")
    )
})

## --- buildClassDepData / buildClassDepGraph: main path ---------------------

test_that("buildClassDepData returns a parent/child edge data.frame", {
    d <- buildClassDepData("RangedSummarizedExperiment")
    expect_s3_class(d, "data.frame")
    expect_true(all(
        c("parent", "child", "type", "childUnion") %in% colnames(d)
    ))
    expect_gt(nrow(d), 0L)
    ## default excludes union-typed child rows
    expect_false(any(d$childUnion))
    ## the class itself should appear as a parent somewhere in the hierarchy
    expect_true("RangedSummarizedExperiment" %in% d$parent)
})

test_that("buildClassDepData(includeUnions = TRUE) includes union rows", {
    d_no_union <- buildClassDepData("RangedSummarizedExperiment")
    d_union <- buildClassDepData(
        "RangedSummarizedExperiment", includeUnions = TRUE
    )
    expect_gte(nrow(d_union), nrow(d_no_union))
})

test_that("buildClassDepGraph returns an igraph built from buildClassDepData", {
    g <- buildClassDepGraph("RangedSummarizedExperiment")
    expect_true(igraph::is_igraph(g))
    expect_true("RangedSummarizedExperiment" %in% names(igraph::V(g)))
    expect_true(all(
        c("type", "parentVirtual", "parentUnion", "childVirtual", "childUnion") %in%
            igraph::edge_attr_names(g)
    ))
})

test_that("buildClassDepData/-Graph error for an undefined class", {
    expect_error(buildClassDepData("NotAClassXYZ"), "is not a defined class")
    expect_error(buildClassDepGraph("NotAClassXYZ"), "is not a defined class")
})

## --- buildClassDepFromPackage ------------------------------------------------

test_that("buildClassDepFromPackage builds dep data for every exported S4 class", {
    res <- buildClassDepFromPackage("Biobase")
    expect_type(res, "list")
    expect_gt(length(res), 0L)
    expect_true("ExpressionSet" %in% names(res))
    expect_s3_class(res[["ExpressionSet"]], "data.frame")
})

## --- plotting functions: verify they run without error on a null device ---

test_that("plotClassDepGraph / plotClassDepData / plotClassDep draw without error", {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)

    g <- buildClassDepGraph("RangedSummarizedExperiment")
    expect_invisible(plotClassDepGraph(g))

    d <- buildClassDepData("RangedSummarizedExperiment")
    expect_invisible(plotClassDepData(d))

    expect_invisible(plotClassDep("RangedSummarizedExperiment"))
})

test_that("plotClassDepGraph errors when required edge attributes are missing", {
    g <- igraph::graph_from_data_frame(data.frame(from = "A", to = "B"))
    expect_error(
        plotClassDepGraph(g),
        "must have the following edge attributes"
    )
})
