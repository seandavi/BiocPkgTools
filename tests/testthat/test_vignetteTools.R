context("vignetteTools")
library(BiocPkgTools)

test_that("getBiocVignette works", {
    skip_if_bioc_offline()
    bpkgl <- biocPkgList()
    vf <- getBiocVignette(bpkgl$vignettes[[1]][1])
    expect_true(file.exists(vf))
})
