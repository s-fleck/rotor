context("BackupTrail")


dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("BackupTrail works as expected", {
  tf <- file.path(td, "test")
  file.create(tf)

  bt <- BackupTrailIndexed$new(tf)
  expect_identical(bt$file, tf)
  expect_identical(bt$backup_dir, dirname(tf))
})


test_that("BackupTrail works as expected for files without extension", {
  tf <- file.path(td, "test")
  file.create(tf)
  bt <- BackupTrailIndexed$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tf, c(".1", ".2", ".3"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  bt$prune(2)
  expect_identical(bt$backups, bus[1:2])
  bt$prune(0)
  expect_identical(bt$backups, character(0))
  file.remove(tf)
})



test_that("BackupTrail works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupTrailIndexed$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log", ".2.log", ".3.log"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  bt$prune(2)
  expect_identical(bt$backups, bus[1:2])
  bt$prune(0)
  expect_identical(bt$backups, character(0))
  file.remove(tf)
})
