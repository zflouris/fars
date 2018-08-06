library(testthat)
context("FARS_tests")

test_that("test_make_filename",{
  path<-make_filename("2013")
  expect_that(path,is_a("string"))
} )
