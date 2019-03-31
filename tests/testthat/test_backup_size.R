context("rotate_size")


test_that("rotate_size works as expected", {
  expect_identical(parse_size(123), 123L)
  expect_identical(parse_size("2k"), 2048L)
})



test_that("parse_info_unit works", {
  parse_info_unit("k")
  parse_info_unit("m")
  expect_error(parse_info_unit("r"))
  expect_identical(parse_size("1k"), 1024L)
})

