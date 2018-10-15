context("githubURLParts")
library(BiocPkgTools)

urls = c("https://github.com/kevinrue/TVTB",
         "https://github.com/ronammar/zFPKM/",
         "https://github.com/ronammar/zFPKM/extrastuff/abc")
ghp = githubURLParts(urls)


test_that("returns data frame and columns correctly", {
    expect_equal(nrow(ghp), 3)
    expect_equal(ncol(ghp), 4)
})

test_that("handles extra stuff in URL appropriately", {
    expect_equivalent(ghp[3, 4, drop=TRUE], 'zFPKM')
    expect_equivalent(ghp[2, 4, drop=TRUE], 'zFPKM')
})
