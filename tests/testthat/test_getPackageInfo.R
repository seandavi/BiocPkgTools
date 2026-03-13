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

test_that(".extract_fnd() returns NA for no funder", {
    expect_identical(
        BiocPkgTools:::.extract_fnd(NA_character_),
        NA_character_
    )
    expect_identical(
        BiocPkgTools:::.extract_fnd(""),
        NA_character_
    )
    expect_identical(
        BiocPkgTools:::.extract_fnd("person('Jane', 'Doe', role = 'aut')"),
        NA_character_
    )
})

test_that(".extract_fnd() extracts single funder", {
    aar <- "c(person('Jane', 'Doe', role = 'aut'),
              person('Big Funder', role = 'fnd'))"
    result <- BiocPkgTools:::.extract_fnd(aar)
    expect_identical(result, "Big Funder")
})

test_that(".extract_fnd() extracts multiple funders", {
    aar <- "c(person('Jane', 'Doe', role = 'aut'),
              person('Funder', 'One', role = 'fnd'),
              person('Funder', 'Two', role = 'fnd'))"
    result <- BiocPkgTools:::.extract_fnd(aar)
    expect_length(result, 2L)
    expect_identical(result, c("Funder One", "Funder Two"))
})

test_that(".extract_fnd() returns NA for malformed input", {
    expect_identical(
        BiocPkgTools:::.extract_fnd("not valid R {{{"),
        NA_character_
    )
})
