# Load data
setwd(system.file("extdata", package = "fars"))


# Test 1
test_that("Check number of rows for 2013 accidents file", {
  testthat::expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
})


# Test 2
test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "fars")),
                         c("accident_2013.csv.bz2",
                           "accident_2014.csv.bz2",
                           "accident_2015.csv.bz2"))
})
