context("copy_or_compress")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("copy_or_compress works as expected", {
  tf <- file.path(td, "compresstest.log")
  saveRDS(iris, file = tf, compress = FALSE)

  r <- copy_or_compress(tf, tf, compression = TRUE)
  expect_true(file.exists(r))
  expect_identical(zip::zip_list(r)[1, ]$filename, "compresstest.log")
  unlink(r)

  r <- copy_or_compress(tf, tf, compression = "base::zip")
  expect_true(file.exists(r))
  expect_identical(zip::zip_list(r)[1, ]$filename, "compresstest.log")
  expect_true(file.remove(tf))
})
