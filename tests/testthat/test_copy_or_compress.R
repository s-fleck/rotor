context("copy_or_compress")

dr <- tempdir()
td <- file.path(dr, "rotor")
timestamp_tolerance <- 10  # seconds

dir.create(td, recursive = TRUE)
teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



create_testfile <- function(){
  tf <- file.path(td, "compresstest.log")
  saveRDS(iris, file = tf, compress = FALSE)
  fake_time <- as.POSIXct("1990-01-01 02:03:04")
  Sys.setFileTime(tf, fake_time)
  expect_true(equalish(file.info(tf)$mtime, fake_time, timestamp_tolerance), "cannot fake timestamp")
  tf
}




test_that("copy_or_compress works with default zip command", {
  skip_if_not(is_zipcmd_available(), "system zip-command is available")

  tf <- file.path(td, "compresstest.log")
  on.exit(unlink(tf))
  saveRDS(iris, file = tf, compress = FALSE)

  r <- copy_or_compress(tf, tf, compression = TRUE)
  on.exit(unlink(r), add = TRUE)
  expect_true(file.exists(r))
  identical(unzip(r, list = TRUE)[["Name"]], "compresstest.log")
})




test_that("copy_or_compress works with internal zip command", {
  skip_if_not(is_zipcmd_available(), "system zip-command is available")

  tf <- file.path(td, "compresstest.log")
  on.exit(unlink(tf))
  saveRDS(iris, file = tf, compress = FALSE)

  r <- copy_or_compress(tf, tf, compression = "utils::zip")
  on.exit(unlink(r), add = TRUE)
  expect_true(file.exists(r))
  identical(unzip(r, list = TRUE)[["Name"]], "compresstest.log")
})




test_that("copy_or_compress works with zip::zipr", {
  skip_if_not_installed("zip")
  skip_if_not(is_zipcmd_available(), "system zip-command is available")

  tf <- file.path(td, "compresstest.log")
  on.exit(unlink(tf))
  saveRDS(iris, file = tf, compress = FALSE)

  r <- copy_or_compress(tf, tf, compression = "zip::zipr")
  on.exit(unlink(r), add = TRUE)
  expect_true(file.exists(r))
  expect_identical(zip::zip_list(r)[1, ]$filename, "compresstest.log")
})




test_that("copy_or_compress preserves timestamp", {
  skip_if_not_installed("zip")
  skip_if_not(is_zipcmd_available(), "system zip-command is available")

  tf <- create_testfile()
  on.exit(unlink(tf))

  copy <- copy_or_compress(tf, paste0(tf, ".copy"))
  on.exit(unlink(copy), add = TRUE)
  expect_true(equalish(file.mtime(tf), file.mtime(copy), tolerance = timestamp_tolerance))

  zip <- copy_or_compress(tf, tf, compression = "utils::zip")
  on.exit(unlink(zip), add = TRUE)
  expect_true(equalish(file.mtime(zip), file.mtime(tf), timestamp_tolerance))
  unlink(zip)

  zip <- copy_or_compress(tf, tf, compression = "zip::zipr")
  expect_true(equalish(file.mtime(zip), file.mtime(tf), timestamp_tolerance))
})


