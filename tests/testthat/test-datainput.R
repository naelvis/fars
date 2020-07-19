library(testthat)

test_that("data input", {
  expect_warning(fars_read_years(2020))
})
