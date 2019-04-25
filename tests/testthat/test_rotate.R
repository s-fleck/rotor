context("rotate_size")




test_that("parse_info_unit works", {
  expect_identical(parse_info_unit("k"), 1024)
  expect_identical(parse_info_unit("m"), 1024 * 1024)
  expect_identical(parse_size(123), 123L)
  expect_error(parse_info_unit("r"))
  expect_identical(parse_size("1k"), 1024)
  expect_equal(parse_size("1.5g"), 1024L^3 * 1.5)
})
