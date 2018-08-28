context("vignetteTools")
library(BiocPkgTools)

bpkgl = biocPkgList()
vf = getBiocVignette(bpkgl$vignettes[[1]][1])


test_that("getBiocVignette works", {
    expect_true(file.exists(vf))
})
