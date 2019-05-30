context("rotate_time")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("backup_time common usecases", {
  tf <- file.path(td, "test.log")
  file.create(
    tf,
    file.path(td, "test.2019-02-01--12-00-00.log")
  )
  on.exit(unlink(tf))
  writeLines("test", tf)
  bq <- BackupQueueDateTime$new(tf, cache_backups = FALSE)

  backup_time(tf, age = "1 month", now = "2019-02-28--12-30-00")
  expect_identical(bq$n_backups, 1L)

  backup_time(tf, age = "1 month", now = "2019-03-01--00-00-00")
  expect_identical(bq$n_backups, 2L)

  file.create(file.path(td, "test.2019-01-01.log"))
  file.create(file.path(td, "test.2018-12-31.log"))
  expect_identical(bq$n_backups, 4L)
  backup_time(tf, max_backups = "1 year", now = "2019-03-01--00-00-01")
  expect_identical(bq$n_backups, 4L)
  expect_equal(bq$last_rotation, as.POSIXct("2019-03-01 00:00:01"))
  expect_identical(
    as.character(min(bq$backups$timestamp)),
    "2019-01-01"
  )

  BackupQueue$new(tf)$prune(0)
})




test_that("backup_time works with timestamps", {
  tf <- file.path(td, "test.log")
  file.create(
    tf,
    file.path(td, "test.2019-02-01--12-00-00.log")
  )
  writeLines("test", tf)
  on.exit(unlink(tf))

  bq <- BackupQueueDateTime$new(tf)
  expect_identical(bq$n_backups, 1L)

  backup_time(tf, age = as.POSIXct("2019-02-01 11:59:59"))
  expect_identical(bq$n_backups, 1L)

  backup_time(tf, age = as.POSIXct("2019-02-01 12:00:00"))
  expect_identical(bq$n_backups, 1L)

  now <- as.POSIXct("2020-01-01 11:59:59")

  tres <- backup_time(tf, age = as.POSIXct("2019-02-01 12:00:01"), now = now)
  expect_match(
    newest_backup(tres),
    "2020-01-01"
  )
  expect_identical(bq$n_backups, 2L)

  expect_equal(min(bq$backups$timestamp), as.POSIXct("2019-02-01 12:00:00"))
  expect_equal(max(bq$backups$timestamp), now)

  backup_time(tf, max_backups = 0)
})




test_that("backup/rotate date works to different directory", {
  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  on.exit(unlink(c(tf, bu_dir), recursive = TRUE))

  file.create(tf)
  writeLines("foobar", tf)

  backup_time(tf, backup_dir = bu_dir, now = as.POSIXct("2019-01-01 12:12:12"), verbose = TRUE, age = "-99999 years")

  expect_identical(
    readLines(tf),
    readLines(file.path(dirname(tf), "backups", "test.2019-01-01--12-12-12.log"))
  )

  expect_identical(n_backups(tf, backup_dir = bu_dir), 1L)
  prune_backups(tf, 0, backup_dir = bu_dir)
  expect_identical(n_backups(tf, backup_dir = bu_dir), 0L)
  expect_length(list.files(bu_dir), 0)
})




test_that("backup/rotate_date works with size", {
  tf     <- file.path(td, "test.log")
  on.exit(unlink(tf))
  saveRDS(iris, tf)
  size_ori <- file.size(tf)

  rotate_time(tf, size = "5kb")
  expect_identical(n_backups(tf), 0L)
  expect_equal(file.size(tf), size_ori)

  rotate_time(tf, size = "0.5kb")
  expect_identical(n_backups(tf), 1L)
  expect_equal(file.size(tf), 0)

  prune_backups(tf, 0)
})




test_that("backup/rotate_time fails if backup already exists for that period", {
  tf <- file.path(td, "test.log")
  on.exit(unlink(tf))
  saveRDS(iris, tf)

  now <- Sys.time()
  backup_time(tf, now = now)
  expect_error(backup_time(tf, now = now), "exists")
  expect_error(rotate_time(tf, now = now, dry_run = TRUE), "exists")

  prune_backups(tf, 0)
})
