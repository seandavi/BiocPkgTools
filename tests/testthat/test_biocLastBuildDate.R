context("Check biocLastBuildDate")

test_that("biocLastBuildDate returns a character string with a name", {
    expect_true(
        length(biocLastBuildDate()) >= 30L
    )
    expect_true(
        all(
            c(paste0("3", ".", 14:0), paste0("2", ".", 14:0)) %in%
                names(biocLastBuildDate())
        )
    )
    expect_identical(
        biocLastBuildDate("3.14"),
        c("3.14" = "2022-04-13")
    )
})
