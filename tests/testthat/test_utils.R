context("utils")


test_that("utils works as expected", {
  expect_identical(fmt_bytes(0), "0 B")
  expect_identical(fmt_bytes(1024), "1 KiB")
  expect_identical(fmt_bytes(2^20), "1 MiB")
  expect_identical(fmt_bytes(2^30), "1 GiB")
  expect_identical(fmt_bytes(2^40), "1 TiB")
  expect_identical(fmt_bytes(2^50), "1024 TiB")
})
