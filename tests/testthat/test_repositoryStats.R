test_that("repositoryStats validates the 'version' argument", {
    expect_error(
        repositoryStats(version = "not-a-version"),
        "'version' is not 'release', 'devel', or a valid 'package_version'"
    )
})

test_that("repositoryStats summarizes the software repository", {
    skip_if_bioc_offline()

    ## On a non-container development machine, BiocManager::containerRepository()
    ## typically resolves to character(0) -- exercise that "no binary repo"
    ## branch explicitly so the test is fast and does not depend on being run
    ## inside a Bioconductor Docker container.
    stats <- repositoryStats(version = "release", binary_repository = character(0))

    expect_s3_class(stats, "repositoryStats")
    expect_true(is.na(stats[["container"]]))
    expect_s3_class(stats[["bioconductor_version"]], "package_version")
    expect_true(is.na(stats[["bioconductor_binary_repository"]]))
    expect_false(stats[["repository_exists"]])
    expect_gt(stats[["n_software_packages"]], 1000)
    expect_identical(stats[["n_binary_packages"]], 0L)
    expect_identical(stats[["n_binary_software_packages"]], 0L)
    expect_identical(
        length(stats[["missing_binaries"]]), stats[["n_software_packages"]]
    )
    expect_null(stats[["out_of_date_binaries"]])
})

test_that("print.repositoryStats formats a summary without erroring", {
    ## Fabricate a small object so this test is instantaneous and independent
    ## of network access -- exercises print.repositoryStats() and the
    ## .repositoryStats_package_format() helper directly.
    fake <- list(
        container = NA_character_,
        bioconductor_version = as.package_version("3.21"),
        bioconductor_binary_repository = NA_character_,
        PACKAGES_mtime = NA_character_,
        query_timestamp = "2025-01-01 00:00 UTC",
        repository_exists = TRUE,
        n_software_packages = 3L,
        n_binary_packages = 2L,
        n_binary_software_packages = 2L,
        missing_binaries = c("pkgA"),
        out_of_date_binaries = c("pkgB", "pkgC")
    )
    class(fake) <- c("repositoryStats", class(fake))

    output <- capture.output(print(fake))
    expect_true(any(grepl("Bioconductor version: 3.21", output, fixed = TRUE)))
    expect_true(any(grepl("Missing binary software packages: 1", output)))
    expect_true(any(grepl("^\\s*pkgA\\s*$", output)))
    expect_true(any(grepl("pkgB pkgC", output)))
})

test_that(".repositoryStats_package_format sorts and wraps package names", {
    formatted <- BiocPkgTools:::.repositoryStats_package_format(
        c("zPkg", "aPkg", "mPkg")
    )
    expect_type(formatted, "character")
    ## sorted alphabetically
    expect_true(
        grepl("aPkg\\s+mPkg\\s+zPkg", formatted)
    )
    ## trailing newline as per implementation
    expect_true(grepl("\n$", formatted))
})
