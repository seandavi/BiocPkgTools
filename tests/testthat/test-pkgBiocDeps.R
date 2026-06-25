test_that("pkgBiocRevDeps(recursive = TRUE) returns only reverse deps", {
    ## Regression test for issue #81: recursive reverse dependencies must not
    ## leak forward (non-reverse) dependencies. Recursing reverse dependencies
    ## over 'Suggests' / 'Enhances' previously exploded to nearly the entire
    ## repository, pulling in forward dependencies of the queried package.
    pkg <- "Rarr"

    db <- utils::available.packages(repos = BiocManager::repositories())
    skip_if_not(pkg %in% rownames(db), "Rarr not available in repositories")

    ## Forward dependencies that are not themselves reverse dependencies should
    ## never appear in the recursive reverse-dependency result.
    fwd <- tools::package_dependencies(pkg, db, which = "all")[[pkg]]
    rev1 <- tools::package_dependencies(
        pkg, db, reverse = TRUE, which = "all"
    )[[pkg]]
    fwd_only <- setdiff(fwd, rev1)

    res <- pkgBiocRevDeps(pkg, recursive = TRUE, only.bioc = FALSE)
    rec <- as.character(res)

    expect_false(any(fwd_only %in% rec))
    ## The explosion produced tens of thousands of packages; a sane recursive
    ## reverse-dependency set is a small fraction of the repository.
    expect_lt(length(rec), nrow(db) / 2)
})
