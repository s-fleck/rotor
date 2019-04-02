context("BackupQueue")




dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("BackupQueue works as expected", {
  tf <- file.path(td, "test")
  file.create(tf)

  bt <- BackupQueue$new(tf)
  expect_identical(bt$file, tf)
  expect_identical(bt$backup_dir, dirname(tf))
  file.remove(tf)
})




test_that("BackupQueue$backup_matrix works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs, ".log")
  file.create(bus)

  expect_setequal(bt$backups, bus)
  expect_setequal(bt$backup_matrix[, "sfx"], sfxs)
  expect_setequal(bt$backup_matrix[, "ext"], "log")
})
