library(testthat)

test_that("data input", {
  is_a("list")(fars_read_years(2013))
})
