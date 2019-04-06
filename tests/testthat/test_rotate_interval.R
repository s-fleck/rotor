context("backup_time")


dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("backup_time warns if indexed backups exist", {
  tf <- file.path(td, "test.log")
  bq <- BackupQueueDate$new(tf)

  file.create(c(
    tf,
    file.path(td, "test.1.log"),
    file.path(td, "test.2.log"),
    file.path(td, "test.2017.log"),
    file.path(td, "test.201701.log"),
    file.path(td, "test.20170201.log"),
    file.path(td, "test.2017-03.log"),
    file.path(td, "test.2017-04-01.log")
  ))

  expect_warning(
    bu <- backup_time(tf),
    "test\\.1\\.log.*test\\.2\\.log"
  )

  BackupQueue$new(tf)$prune(0)
  file.remove(tf)
})




test_that("backup_time works as expected for years", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  # no backup younger than 1 year exists, so rotate
  bu <- backup_time(tf, "1 year")
  expect_true(file.size(bu) > 1)

  bq <- BackupQueueDate$new(tf)
  expect_true(bq$has_backups)
  bq$prune(0)

  # ensure backup_time believes it is 2019-01-01
  mockery::stub(backup_time, "Sys.Date", as.Date("2019-01-01"))

  # no backup because last backup is less than a year old
  file.create(file.path(td, "test.2019-12-31.log"))
  bu <- backup_time(tf, "1 year")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # roate because backup is from last year
  file.create(file.path(td, "test.2018-12-31.log"))
  bu <- backup_time(tf, "2 year")  # dont rotate
  expect_identical(bq$n_backups, 1L)
  bu <- backup_time(tf, "1 year")  # rotate
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("backup_time works as expected for quarters", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf)

  # no backup younger than 1 quarter exists, so rotate
  bu <- backup_time(tf, "1 quarter")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # ensure backup_time believes it is 2019-01-01
  mockery::stub(backup_time, "Sys.Date",    as.Date("2019-04-01"))

  # no backup because last backup is less than a quarter old
  file.create(file.path(td, "test.2019-06-21.log"))
  bu <- backup_time(tf, "1 quarter")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 quarter old
  file.create(file.path(td, "test.2019-01-01.log"))
  bu <- backup_time(tf, "2 quarter")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 quarter old
  bu <- backup_time(tf, "1 quarter")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})



test_that("backup_time works as expected for months", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf)

  # no backup younger than 1 month exists, so rotate
  bu <- backup_time(tf, "1 month")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # ensure backup_time believes it is 2019-01-01
  mockery::stub(backup_time, "Sys.Date",    as.Date("2019-05-02"))

  # no backup because last backup is less than a month old
  file.create(file.path(td, "test.2019-05-21.log"))
  bu <- backup_time(tf, "1 month")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 month old
  file.create(file.path(td, "test.2019-04-21.log"))
  bu <- backup_time(tf, "2 month")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 month old
  bu <- backup_time(tf, "1 month")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("backup_time works as expected for weeks", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf)

  # no backup younger than 1 week exists, so rotate
  bu <- backup_time(tf, "1 week")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # ensure backup_time believes it is 2019-01-01
  mockery::stub(backup_time, "Sys.Date",    as.Date("2019-01-30"))  # first of week is 2019-01-28

  # no backup because last backup is less than a week old
  file.create(file.path(td, "test.2019-01-28.log"))
  bu <- backup_time(tf, "1 week")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 week old
  file.create(file.path(td, "test.2019-01-27.log"))
  bu <- backup_time(tf, "2 week")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 week old
  bu <- backup_time(tf, "1 week")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("parse interval", {
  expect_identical(parse_interval(9)$unit, "day")
  expect_identical(parse_interval("1 week")$unit, "week")
  expect_identical(parse_interval("2 months")$unit, "month")
  expect_identical(parse_interval("3 quarters")$unit, "quarter")
  expect_identical(parse_interval("4 years")$unit, "year")
})
