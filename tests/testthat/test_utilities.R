context("utilities")

test_that(".get_remote_size works", {
    skip_if_offline()
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"
    size <- BiocPkgTools:::.get_remote_size(url)
    expect_type(size, "double")
    expect_gt(size, 0)

    # Check invalid URL
    expect_identical(
        BiocPkgTools:::.get_remote_size(
            "https://invalid.url.example.com/file.tab"
        ),
        NA_integer_
    )
})

test_that(".cache_url_file works", {
    skip_if_offline()
    url <- "https://bioconductor.org/packages/stats/bioc/bioc_pkg_stats.tab"
    path <- BiocPkgTools:::.cache_url_file(url)
    expect_true(file.exists(path))
})
