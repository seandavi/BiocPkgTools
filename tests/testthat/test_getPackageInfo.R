context("getPackageInfo")
library(BiocPkgTools)

test_that("getPackageInfo", {
    expect_error(getPackageInfo("../test"),
                 'dir "../test" does not exist')
    expect_error(getPackageInfo("../"),
                 'dir "../" does not contain')
    actual <- getPackageInfo(system.file(package = "BiocManager"))
    expect_true(is.matrix(actual))
    expect_type(actual,"character")
    expect_equal(colnames(actual),c("Package","Author"))
})

test_that("getPackageInfo() parses Authors@R", {
    fl <- tempfile()
    dir.create(fl)

    writeLines(c(
        "Package: test",
        "Authors@R: person('Im A.', 'Author')"
    ), file.path(fl, "DESCRIPTION"))
    expect_identical(getPackageInfo(fl)[,"Author"], c(Author = "Im A. Author"))

    writeLines(c(
        "Package: test",
        "Authors@R: person('Iman', 'Author')"
    ), file.path(fl, "DESCRIPTION"))
    expect_identical(getPackageInfo(fl)[,"Author"], c(Author = "Iman Author"))

    writeLines(c(
        "Package: test",
        "Authors@R: c(person('Iman', 'Author'), person('Im A.', 'Author'))"
    ), file.path(fl, "DESCRIPTION"))
    aut <- getPackageInfo(fl)[,"Author"]
    expect_identical(aut, c(Author = "Iman Author, Im A. Author"))
})
