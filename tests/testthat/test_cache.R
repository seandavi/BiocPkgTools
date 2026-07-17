context("cache")

## these tests never touch the network -- pure local caching logic.
## the pkgToolsCache option is saved/restored around each test so that
## these tests can't leak state into other test files.

withr_reset_option <- function(expr) {
    old <- getOption("pkgToolsCache")
    on.exit(options(pkgToolsCache = old), add = TRUE)
    force(expr)
}

test_that("setCache creates a new cache directory and sets the option", {
    withr_reset_option({
        dir <- tempfile("bpt_cache_")
        expect_false(dir.exists(dir))

        res <- setCache(directory = dir, verbose = FALSE, ask = FALSE)

        expect_true(dir.exists(dir))
        expect_identical(res, dir)
        expect_identical(getOption("pkgToolsCache"), dir)

        unlink(dir, recursive = TRUE)
    })
})

test_that("setCache validates its 'directory' argument", {
    expect_error(setCache(directory = c("a", "b")))
    expect_error(setCache(directory = 123))
    expect_error(setCache(directory = NA_character_))
})

test_that("setCache errors for a missing directory when ask = TRUE and non-interactive", {
    withr_reset_option({
        dir <- tempfile("bpt_cache_missing_")
        expect_error(
            setCache(directory = dir, verbose = FALSE, ask = TRUE),
            "not created"
        )
        expect_false(dir.exists(dir))
    })
})

test_that("pkgToolsCache returns the currently-set cache directory", {
    withr_reset_option({
        dir <- tempfile("bpt_cache_")
        setCache(directory = dir, verbose = FALSE, ask = FALSE)
        expect_identical(pkgToolsCache(), dir)
        unlink(dir, recursive = TRUE)
    })
})

test_that(".getAnswer returns 'n' in a non-interactive session", {
    ## interactive() is always FALSE under `R CMD check`/testthat, so
    ## .getAnswer always short-circuits to "n" regardless of `allowed`
    expect_identical(BiocPkgTools:::.getAnswer("prompt? ", c("y", "n")), "n")
})

test_that(".get_cache returns a BiocFileCache built on the pkgToolsCache option", {
    withr_reset_option({
        dir <- tempfile("bpt_cache_")
        options(pkgToolsCache = dir)

        bfc <- BiocPkgTools:::.get_cache()
        expect_s4_class(bfc, "BiocFileCache")
        expect_true(dir.exists(dir))

        unlink(dir, recursive = TRUE)
    })
})
