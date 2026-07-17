## dataciteXMLGenerate() is pure local XML generation (no network, no
## credentials) -- an easy, fast win for coverage.

test_that("dataciteXMLGenerate builds the expected DataCite XML structure", {
    x <- BiocPkgTools:::dataciteXMLGenerate("BiocPkgTools")

    expect_s3_class(x, "xml_document")

    root <- xml2::xml_root(x)
    expect_identical(xml2::xml_name(root), "resource")
    expect_identical(
        xml2::xml_attr(root, "xmlns"),
        "http://datacite.org/schema/kernel-3"
    )

    idnode <- xml2::xml_find_first(x, "//identifier")
    expect_identical(xml2::xml_attr(idnode, "identifierType"), "DOI")
    expect_identical(
        xml2::xml_text(idnode), "doi:10.5072/FK2.bioc.BiocPkgTools"
    )

    descnode <- xml2::xml_find_first(x, "//descriptions/description")
    expect_identical(xml2::xml_text(descnode), "this is the description")
})

test_that("dataciteXMLGenerate interpolates the package name for other pkgs", {
    x <- BiocPkgTools:::dataciteXMLGenerate("SomeOtherPkg")
    idnode <- xml2::xml_find_first(x, "//identifier")
    expect_identical(
        xml2::xml_text(idnode), "doi:10.5072/FK2.bioc.SomeOtherPkg"
    )
})

test_that("dataciteXMLGenerate escapes special XML characters safely", {
    ## edge case: package-like strings containing XML metacharacters should
    ## be safely escaped in the serialized document but recoverable via
    ## xml_text()
    x <- BiocPkgTools:::dataciteXMLGenerate("A&B<pkg>")

    serialized <- as.character(x)
    expect_true(grepl("&amp;", serialized, fixed = TRUE))
    expect_true(grepl("&lt;pkg&gt;", serialized, fixed = TRUE))

    idnode <- xml2::xml_find_first(x, "//identifier")
    expect_identical(
        xml2::xml_text(idnode), "doi:10.5072/FK2.bioc.A&B<pkg>"
    )
})
