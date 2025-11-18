context("biocDownloadStats")

test_that("biocDownloadStats works", {
    res <- biocDownloadStats("software")
    expect_s3_class(res, "bioc_downloads")
    expect_s3_class(res, "tbl_df")
    expect_true(
        all(
            c(
                "Package",
                "Year",
                "Month",
                "Nb_of_distinct_IPs",
                "Nb_of_downloads",
                "Date"
            ) %in%
                names(res)
        )
    )
})

test_that("pkgDownloadStats works", {
    res <- pkgDownloadStats("BiocGenerics")
    expect_s3_class(res, "tbl_df")
    expect_true(
        all(
            c("Year", "Month", "Nb_of_distinct_IPs", "Nb_of_downloads") %in%
                names(res)
        )
    )
})

test_that("pkgDownloadRank works", {
    res <- pkgDownloadRank("BiocGenerics", "software")
    expect_type(res, "double")
    expect_true(!is.na(res))
})

test_that("activitySince works", {
    res <- activitySince(
        "Bioconductor/BiocGenerics",
        "issues",
        "closed",
        "2021-05-01"
    )
    expect_s3_class(res, "tbl_df")
    expect_true(
        all(c("created_at", "number", "title") %in% names(res))
    )
})
