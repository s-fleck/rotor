context("utils")


test_that("utils works as expected", {

  expect_identical(readable_size(0), "0 B")
  expect_identical(readable_size(1024), "1 kB")
  expect_identical(readable_size(2^20), "1 MB")
  expect_identical(readable_size(2^30), "1 GB")
  expect_identical(readable_size(2^40), "1 TB")
  expect_identical(readable_size(2^50), "1024 TB")


  readable_size(file.size(list.files(".")))
})
