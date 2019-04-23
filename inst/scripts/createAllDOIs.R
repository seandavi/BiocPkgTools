library(BiocPkgTools)
pl = biocPkgList()
testing = TRUE # change to FALSE to actually update/create records
z = mapply(
    FUN = BiocPkgTools:::generateBiocPkgDOI,
    pl$Package,
    pl$Author,
    2017,
    testing=testing
)
