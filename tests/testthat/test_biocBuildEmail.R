## biocBuildEmail() can send a real email via 'blastula' when
## `dry.run = FALSE`. We never do that here. Instead we exercise the safe
## `textOnly = TRUE, dry.run = TRUE` path, which builds the message text
## locally and never calls out to an SMTP server. The user-info / mail-log
## BiocFileCache cache is redirected to a temp dir for every test below so
## nothing here reads or writes a developer's real cache.

test_that(".nameCut and .emailCut parse 'Name <email>' maintainer strings", {
    expect_identical(
        BiocPkgTools:::.nameCut("Jane Q. Doe <jane@example.com>"),
        "Jane Q. Doe"
    )
    expect_identical(
        BiocPkgTools:::.emailCut("Jane Q. Doe <jane@example.com>"),
        "jane@example.com"
    )
    ## edge case: "name at domain" style obfuscation used on some CRAN/Bioc
    ## maintainer pages
    expect_identical(
        BiocPkgTools:::.emailCut("Jane Q. Doe <jane at example.com>"),
        "jane@example.com"
    )
    ## edge case: embedded newline in the maintainer field is normalized to a
    ## single space
    expect_identical(
        BiocPkgTools:::.nameCut("Jane Q. Doe\n<jane@example.com>"),
        "Jane Q. Doe"
    )
})

test_that("templatePath resolves each known template file", {
    types <- c(
        "buildemail", "deprecation", "deprecguide", "cranreport", "revdepnote"
    )
    for (type in types) {
        path <- templatePath(type)
        expect_true(file.exists(path))
        expect_true(nzchar(path))
    }
    expect_error(templatePath("not-a-real-type"), "'arg' should be one of")
})

test_that(".getUserInfo caches credentials and ignores later arguments once cached", {
    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    first <- BiocPkgTools:::.getUserInfo(
        "First Name", "first@example.com", "AB00001"
    )
    expect_identical(first[["core.name"]], "First Name")
    expect_identical(first[["core.email"]], "first@example.com")
    expect_identical(first[["core.id"]], "AB00001")

    ## once written, the cache is authoritative -- new arguments are ignored
    second <- BiocPkgTools:::.getUserInfo(
        "Second Name", "second@example.com", "CD00002"
    )
    expect_identical(second, first)
})

test_that(".getMailLog, .checkEntry, .addEntry and sentHistory round-trip a log entry", {
    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    expect_error(sentHistory(), "No log available")

    logfile <- BiocPkgTools:::.getMailLog()
    expect_true(file.exists(logfile))

    ## dry.run short-circuits to FALSE regardless of log contents
    expect_false(
        BiocPkgTools:::.checkEntry(
            logfile, "Jane Doe", "jane@example.com", "somePkg",
            "July 01, 2026", dry.run = TRUE
        )
    )

    ## nothing logged yet => no duplicate found (anyDuplicated returns 0)
    status_before <- BiocPkgTools:::.checkEntry(
        logfile, "Jane Doe", "jane@example.com", "somePkg",
        "July 01, 2026", dry.run = FALSE
    )
    expect_identical(status_before, 0L)

    BiocPkgTools:::.addEntry(
        logfile, "Jane Doe", "jane@example.com", "somePkg",
        "July 01, 2026", resend = FALSE
    )

    hist <- sentHistory()
    expect_s3_class(hist, "data.frame")
    expect_identical(nrow(hist), 1L)
    expect_identical(hist[["maintainer"]], "Jane Doe")
    expect_identical(hist[["package"]], "somePkg")

    ## the same entry now registers as a duplicate
    status_after <- BiocPkgTools:::.checkEntry(
        logfile, "Jane Doe", "jane@example.com", "somePkg",
        "July 01, 2026", dry.run = FALSE
    )
    expect_gt(status_after, 0L)
})

test_that("biocBuildEmail builds a dry-run, text-only build-failure notice", {
    skip_if_bioc_offline()

    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    send <- biocBuildEmail(
        "BiocPkgTools", dry.run = TRUE, textOnly = TRUE,
        core.name = "Test User", core.email = "test@example.com",
        core.id = "TU00001"
    )

    expect_type(send, "character")
    expect_true(any(grepl("BiocPkgTools Bioconductor package", send, fixed = TRUE)))
    expect_true(any(grepl("checkResults/release/bioc-LATEST/BiocPkgTools", send)))
    expect_true(any(grepl("checkResults/devel/bioc-LATEST/BiocPkgTools", send)))
})

test_that("biocBuildEmail errors for a package not found on Bioconductor", {
    skip_if_bioc_offline()

    tmpcache <- tempfile("bpttest_cache_")
    withr::local_options(list(pkgToolsCache = tmpcache))

    expect_error(
        biocBuildEmail(
            "NoSuchBiocPackageXYZ123", version = "release",
            dry.run = TRUE, textOnly = TRUE,
            core.name = "Test User", core.email = "test@example.com",
            core.id = "TU00001"
        ),
        "No pkg .* found on Bioconductor"
    )
})

test_that("biocBuildEmail requires an existing email template file", {
    expect_error(
        biocBuildEmail(
            "BiocPkgTools", emailTemplate = tempfile(fileext = ".Rmd"),
            core.name = "Test User", core.email = "test@example.com",
            core.id = "TU00001"
        ),
        "'emailTemplate' file not found"
    )
})
