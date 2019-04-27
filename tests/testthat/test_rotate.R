context("rotate")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("backup/rotate happy path", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  tf_size <- file.size(tf)
  bq <- BackupQueue$new(tf)

  # no backup because dry run
  expect_message(backup(tf, dry_run = TRUE), "dry_run")
  expect_identical(bq$n_backups, 0L)

  # not rotating because file is to small
  backup(tf, size = 1e6)
  expect_identical(bq$n_backups, 0L)

  # backup
  backup(tf, size = 1)
  expect_identical(bq$n_backups, 1L)

  # backup (zip)
  backup(tf, compression = TRUE)
  expect_identical(bq$n_backups, 2L)
  expect_identical(tools::file_ext(bq$backups$path[[1]]), "zip")

  # not rotating because dryrun
  expect_message(rotate(tf, dry_run = TRUE, compression = TRUE), "dry_run")
  expect_identical(bq$n_backups, 2L)

  # rotating
  rotate(tf, compression = FALSE)
  expect_identical(bq$n_backups, 3L)
  expect_equal(file.size(tf), 0)
  expect_equal(file.size(bq$backups$path[[1]]), tf_size)
  expect_equal(bq$backups$sfx, as.character(1:3))

  bq$prune(0)
  file.remove(tf)
  expect_length(list.files(td), 0)
})




test_that("parse_info_unit works", {
  expect_identical(parse_info_unit("k"), 1024)
  expect_identical(parse_info_unit("m"), 1024 * 1024)
  expect_identical(parse_size(123), 123L)
  expect_error(parse_info_unit("r"))
  expect_identical(parse_size("1k"), 1024)
  expect_equal(parse_size("1.5g"), 1024L^3 * 1.5)
})
