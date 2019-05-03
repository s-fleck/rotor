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

  bq <- BackupQueue$new(tf)
  expect_identical(bq$file, tf)
  expect_identical(bq$backup_dir, dirname(tf))
  file.remove(tf)
})




test_that("BackupQueue finding backups works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs, ".log")
  file.create(bus)

  expect_setequal(bq$backups$path, bus)
  expect_setequal(bq$backups$sfx, sfxs)
  expect_setequal(bq$backups$ext, "log")
  bq$prune(0)
})



test_that("BackupQueue finding backups works as expected for files without extension", {
  tf <- file.path(td, "test")
  file.create(tf)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs)
  file.create(bus)

  expect_setequal(bq$backups$path, bus)
  expect_setequal(bq$backups$sfx, sfxs)
  expect_setequal(bq$backups$ext, "")
})




test_that("drurun/verbose prune", {
  tf <- file.path(td, "test")
  file.create(tf)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs)
  file.create(bus)

  # testing the option tests the function argument at the same time
  withr::with_options(
    list(rotor.dry_run = TRUE),
    expect_message(invisible(bq$prune(0)))
  )
  withr::with_options(
    list(rotor.dry_run = TRUE, rotor.verbose = FALSE),
    expect_silent(invisible(bq$prune(0)))
  )

  expect_identical(bq$n_backups, length(sfxs))

  expect_message(bq$prune(0, verbose = TRUE), "test.1")
  expect_message(bq$prune(0, verbose = TRUE), "Nothing")
  expect_identical(bq$n_backups, 0L)
})
