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

  bt <- BackupTrail$new(tf)
  expect_identical(bt$file, tf)
  expect_identical(bt$backup_dir, dirname(tf))
  file.remove(tf)
})
