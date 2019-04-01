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

  # find backups
  expect_identical(bt$backups, character(0))

  bus <- paste0(tf, c(".1", ".2", ".3"))
  file.create(bus)

  expect_identical(bt$backups, bus)
})
