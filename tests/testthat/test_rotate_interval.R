context("rotate_interval")


test_that("rotate_interval works as expected", {


})





test_that("parse interval", {

  expect_identical(parse_interval(9)$unit, "day")
  expect_identical(parse_interval("1 week")$unit, "week")
  expect_identical(parse_interval("2 months")$unit, "month")


})
