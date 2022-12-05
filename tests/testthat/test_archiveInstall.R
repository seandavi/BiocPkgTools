test_that("'archiveInstall' errors when version not supported", {
    version <- "0.99"
    expect_error(
        archiveInstall(version = version)
    )
})

test_that("'archiveInstall' errors when snapshot is wrong", {
    version <- "3.14"
    expect_error(
        archiveInstall(version = version, snapshot = "NULL")
    )
    expect_error(
        archiveInstall(version = version, snapshot = c("RSPM", "MRAN"))
    )
})


