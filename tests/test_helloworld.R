library(testthat)
context("test helloworld")
test_that("test helloworld",{
  printstr <- "hi chencheng"
  expect_that(sayHi("chencheng"),equals(printstr))
})
#single file unittest

