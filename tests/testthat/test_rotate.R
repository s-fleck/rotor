context("roate")

td <- file.path(tempdir(), "rotor")
dir.create(td, recursive = TRUE)

teardown(unlink(td, recursive = TRUE))

test_that("roate keeps expected number of backups", {
  tf <- file.path(td, "test.log")
  file.create(tf)

  for (i in 1:10)  backup(tf, max_backups = 5)
  expect_length(find_children(tf), 5)

  for (i in 1:50)  backup(tf, max_backups = 12)
  expect_length(find_children(tf), 12)

  file.remove(list.files(td, full.names = TRUE))
})



test_that("compressed rotate works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)

  for (i in 1:10)  backup(tf, max_backups = 10)
    expect_length(find_children(tf), 10)

  for (i in 1:5)
    x <- backup(tf, max_backups = 10, compression = "zip")

  expect_length(find_children(tf), 10)
  expect_identical(
    tools::file_ext(find_children(tf)),
    c(rep("zip", 5), rep("log", 5))
  )
})

