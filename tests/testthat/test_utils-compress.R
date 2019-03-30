context("utils-compress")


test_that("utils-compress works as expected", {
  td <- file.path(tempdir(), "rotor")
  dir.create(td, recursive = TRUE)
  teardown(unlink(td, recursive = TRUE))

  tf <- file.path(td, "compresstest.log")
  saveRDS(iris, file = tf, compress = FALSE)

  r <- compress_and_remove(tf, remove = FALSE)
  expect_true(file.exists(r))
  expect_identical(zip::zip_list(r)[1, ]$filename, "compresstest.log")
  unlink(r)

  r <- compress_and_remove(tf, compression = "zip_base")
  expect_true(file.exists(r))
  expect_identical(zip::zip_list(r)[1, ]$filename, "compresstest.log")
  expect_false(file.exists(tf))
})
