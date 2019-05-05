context("copy_or_compress")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("copy_or_compress works with default zip command", {
  tf <- file.path(td, "compresstest.log")
  on.exit(unlink(tf))
  saveRDS(iris, file = tf, compress = FALSE)

  r <- copy_or_compress(tf, tf, compression = TRUE)
  on.exit(unlink(r), add = TRUE)
  expect_true(file.exists(r))
  identical(unzip(r, list = TRUE)[["Name"]], "compresstest.log")
})




test_that("copy_or_compress works with internal zip command", {
  tf <- file.path(td, "compresstest.log")
  on.exit(unlink(tf))
  saveRDS(iris, file = tf, compress = FALSE)

  r <- copy_or_compress(tf, tf, compression = "base::zip")
  on.exit(unlink(r), add = TRUE)
  expect_true(file.exists(r))
  identical(unzip(r, list = TRUE)[["Name"]], "compresstest.log")
})




test_that("copy_or_compress works with zip::zipr", {
  tf <- file.path(td, "compresstest.log")
  on.exit(unlink(tf))
  saveRDS(iris, file = tf, compress = FALSE)

  r <- copy_or_compress(tf, tf, compression = "zip::zipr")
  on.exit(unlink(r), add = TRUE)
  expect_true(file.exists(r))
  expect_identical(zip::zip_list(r)[1, ]$filename, "compresstest.log")
})
