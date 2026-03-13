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
        BiocPkgTools:::.extract_fnd("Jane Doe [aut, cre]"),
        NA_character_
    )
})

test_that(".extract_fnd() extracts single funder", {
    result <- BiocPkgTools:::.extract_fnd(
        "Jane Doe [aut], Big Funder [fnd]"
    )
    expect_identical(result, "Big Funder [fnd]")
})

test_that(".extract_fnd() extracts multiple funders", {
    result <- BiocPkgTools:::.extract_fnd(
        "Jane Doe [aut], Funder One [fnd], Funder Two [fnd]"
    )
    expect_length(result, 2L)
    expect_identical(result, c("Funder One [fnd]", "Funder Two [fnd]"))
})

test_that(".extract_fnd() extracts funders from multiline Author field", {
    author <- paste0(
        "Martin Morgan [aut, cre],\n",
        "    Chan Zuckerberg Initiative DAF CZF2019-002443 [fnd],\n",
        "    NIH NCI ITCR U24CA180996 [fnd]"
    )
    result <- BiocPkgTools:::.extract_fnd(author)
    expect_length(result, 2L)
    expect_identical(result[[1L]], "Chan Zuckerberg Initiative DAF CZF2019-002443 [fnd]")
    expect_identical(result[[2L]], "NIH NCI ITCR U24CA180996 [fnd]")
})

test_that(".extract_fnd() returns NA when no [fnd] tag present", {
    expect_identical(
        BiocPkgTools:::.extract_fnd("some text without role brackets"),
        NA_character_
    )
})
