context("condaDownloadStats")

test_that("anacondaDownloadStats returns tidy Anaconda download stats", {
    skip_if_offline()

    tbl <- anacondaDownloadStats()

    expect_s3_class(tbl, "bioc_downloads")
    expect_s3_class(tbl, "tbl_df")
    expect_true(all(c(
        "Package", "Year", "Month", "Nb_of_distinct_IPs",
        "Nb_of_downloads", "repo", "Date"
    ) %in% colnames(tbl)))
    expect_gt(nrow(tbl), 0L)

    ## Anaconda does not provide unique-IP counts
    expect_true(all(is.na(tbl[["Nb_of_distinct_IPs"]])))
    expect_true(all(tbl[["repo"]] == "Anaconda"))
    expect_s3_class(tbl[["Date"]], "Date")

    ## a well-known Bioconductor package distributed via Anaconda
    expect_true("DESeq2" %in% tbl[["Package"]])
})
