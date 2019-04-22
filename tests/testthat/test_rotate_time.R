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
  bq <- BackupQueueDateTime$new(tf)

  mockery::stub(backup_time, "Sys.time", as.POSIXct("2019-02-28--12-30-00"))
  backup_time(tf, age = "1 month")
  expect_identical(bq$n_backups, 1L)

  mockery::stub(backup_time, "Sys.time", as.POSIXct("2019-03-01--00-00-00"))
  backup_time(tf, age = "1 month")
  expect_identical(bq$n_backups, 2L)

  file.create(file.path(td, "test.2019-01-01.log"))
  file.create(file.path(td, "test.2018-12-31.log"))
  expect_identical(bq$n_backups, 4L)
  backup_time(tf, n_backups = "1 year", now = as.POSIXct("2019-03-01 00:00:01"))
  expect_identical(bq$n_backups, 4L)
  expect_equal(bq$last_backup, as.POSIXct("2019-03-01 00:00:01"))
  expect_identical(
    as.character(min(bq$backups$timestamp)),
    "2019-01-01"
  )

  BackupQueue$new(tf)$prune(0)
  file.remove(tf)
})




test_that("backup_time works with timestamps", {
  tf <- file.path(td, "test.log")
  file.create(
    tf,
    file.path(td, "test.2019-02-01--12-00-00.log")
  )

  bq <- BackupQueueDateTime$new(tf)
  expect_identical(bq$n_backups, 1L)

  expect_identical(
    backup_time(tf, age = as.POSIXct("2019-02-01 11:59:59")),
    character()
  )
  expect_identical(bq$n_backups, 1L)

  expect_identical(
    backup_time(tf, age = as.POSIXct("2019-02-01 12:00:00")),
    character()
  )
  expect_identical(bq$n_backups, 1L)

  now <- as.POSIXct("2020-01-01 11:59:59")

  tres <- backup_time(tf, age = as.POSIXct("2019-02-01 12:00:01"), now = now)
  expect_match(
    tres,
    "2020-01-01"
  )
  expect_identical(bq$n_backups, 2L)

  expect_equal(min(bq$backups$timestamp), as.POSIXct("2019-02-01 12:00:00"))
  expect_equal(max(bq$backups$timestamp), now)



  backup_time(tf, n_backups = 0)
  file.remove(tf)
})
