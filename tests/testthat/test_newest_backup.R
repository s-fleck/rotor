context("newest_backup")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("first_backup works as expected", {
  tf <- file.path(td, "test.log")
  files <- file.path(
    td,
    c("test.2019-02-01--12-00-00.log", "test.2019-02-01--12-00-01.log")
  )

  file.create(tf, files)

  expect_match(newest_backup(tf), files[[2]])
  expect_match(oldest_backup(tf), files[[1]])

  files2 <- file.path(td, c("test.1.log", "test.2.log"))
  file.create(files2)

  expect_error(newest_backup(tf))
  file.remove(files)

  expect_match(newest_backup(tf), files2[[1]])
  expect_match(oldest_backup(tf), files2[[2]])

  file.remove(tf, files2)
})
