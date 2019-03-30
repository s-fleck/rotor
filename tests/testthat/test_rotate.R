context("roate")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("roate keeps expected number of backups", {
  tf <- file.path(td, "test.log")
  file.create(tf)

  for (i in 1:10)  backup(tf, max_backups = 5)
  expect_length(find_backups(tf), 5)

  for (i in 1:50)  backup(tf, max_backups = 12)
  expect_length(find_backups(tf), 12)

  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})




test_that("compressed rotate works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)

  for (i in 1:10)  backup(tf, max_backups = 10)
    expect_length(find_backups(tf), 10)

  for (i in 1:7)
    x <- backup(tf, max_backups = 10, compression = "zip")

  r <- find_backups(tf)

  expect_length(r, 10)
  expect_identical(
    tools::file_ext(r),
    c(rep("zip", 7), rep("log", 3))
  )

  expect_identical(first(basename(r)), "test.01.log.zip")
  expect_identical(last(basename(r)), "test.10.log")

  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})




test_that("roate file without extension", {
  tf <- file.path(td, "test")
  file.create(tf)

  for (i in 1:10)  backup(tf, max_backups = 5)
  expect_length(find_backups(tf), 5)

  for (i in 1:50)  backup(tf, max_backups = 12)
  expect_length(find_backups(tf), 12)

  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})

