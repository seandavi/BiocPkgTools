context("problemPage")
library(BiocPkgTools)

ppage = problemPage(includeOK = TRUE)
ppage2 = problemPage(includeOK = FALSE)

test_that("problempage is a datatable", {
    expect_is(ppage, 'datatables')
    expect_is(ppage, 'htmlwidget')
})

test_that("data is at least close to correct", {
  expect_gt(nrow(ppage$x$data), 0)
  expect_gte(nrow(ppage$x$data), nrow(ppage2$x$data))
})

test_that("the 'dependsOn' argument can be used", {
  expect_silent(problemPage(authorPattern = "", dependsOn = "limma")) %>%
    expect_is('datatables')
})

test_that("'authorPattern' trumps 'dependsOn'", {
  expect_warning(problemPage(authorPattern = "V.*Carey", dependsOn = "limma")) %>%
    expect_equal(ppage2)
})
