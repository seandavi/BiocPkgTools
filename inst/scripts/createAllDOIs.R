library(BiocPkgTools)
pl = getBiocPkgList()
testing = TRUE # change to FALSE to actually update/create records
z = mapply(FUN = generateBiocPkgDOI,pl$Package,pl$Author,2017,testing=testing)
