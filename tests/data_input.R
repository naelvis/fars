library(testthat)
library(fars)

test_that("data input", {
  is_a("list")(fars_read_years(2013))
  is_a("data.frame")(fars_read_years(2013)[[1]])
})
