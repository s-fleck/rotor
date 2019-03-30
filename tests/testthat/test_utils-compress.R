context("utils-compress")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})


test_that("utils-compress works as expected", {
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
