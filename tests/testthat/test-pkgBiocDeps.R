test_that("pkgBiocRevDeps(recursive = TRUE) returns only reverse deps", {
    skip_if_offline()
    ## Regression test for issue #81: Recursing reverse dependencies over "all"
    ## dependencies will return nearly the entire repository, including some
    ## forward dependencies of the queried package.
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

    res <- pkgBiocRevDeps(
        pkg, which = "all", recursive = TRUE, only.bioc = FALSE
    )
    rec <- as.character(res)

    ## check that some forward dependencies are present in the recursive result
    expect_true(any(fwd_only %in% rec))

    ## check that the recursive result is not too small
    expect_gt(length(rec), nrow(db) / 2)
})
