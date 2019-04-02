context("BackupTrailIndex")




dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("BackupTrailIndex works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupTrailIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log.zip", ".2.log.tar.gz", ".3.log"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  expect_identical(bt$backup_matrix[, "sfx"], as.character(1:3))
  bt$prune(0)
  expect_length(bt$backups, 0)

  # finding and pruning backups works
  bt <- BackupTrailDate$new(tf)
  expect_identical(bt$backups, character(0))
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01.log.zip", ".2019-01-02.log.tar.gz", ".2019-01-03.log"))
  file.create(bus)
  expect_identical(bt$backup_matrix[, "sfx"], c("2019-01-03", "2019-01-02", "2019-01-01"))
  bt$prune(0)
  file.remove(tf)
})




test_that("BackupTrail works as expected for files without extension", {
  tf <- file.path(td, "test")
  file.create(tf)
  bt <- BackupTrailIndex$new(tf)

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
  bt <- BackupTrailIndex$new(tf)

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

