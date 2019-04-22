context("rotate_date")


dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("backup_date warns if indexed backups exist", {
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
    bu <- backup_date(tf),
    "test\\.1\\.log.*test\\.2\\.log"
  )

  BackupQueue$new(tf)$prune(0)
  file.remove(tf)
})




test_that("backup_date examples from documentation", {
  #' When rotating/backing up `"1 months"` means "make a new backup if the last
  #' backup is from the preceeding month". E.g if the last backup of `myfile`
  #' is from `2019-02-01` then `backup_date(myfile, age = "1 month")` will only
  #' create a backup if the current date is at least `2019-03-01`.
  tf <- file.path(td, "test.log")
  file.create(
    tf,
    file.path(td, "test.2019-02-01.log")
  )

  mockery::stub(backup_date, "Sys.Date", as.Date("2019-02-28"))
  backup_date(tf, age = "1 month")
  bq <- BackupQueueDate$new(tf)
  expect_identical(bq$n_backups, 1L)

  mockery::stub(backup_date, "Sys.Date", as.Date("2019-03-01"))
  backup_date(tf, age = "1 month")
  bq <- BackupQueueDate$new(tf)
  expect_identical(bq$n_backups, 2L)

  #' When pruning/limiting backup queues, `"1 year"` means "keep at least most
  #' one year worth of backups". So if you call
  #' `backup_date(myfile, n_backups = "1 year")` on `2019-03-01`, it will create
  #' a backup and then remove all backups of `myfile` before `2019-01-01`.
  mockery::stub(backup_date, "Sys.Date", as.Date("2019-03-02"))
  file.create(file.path(td, "test.2019-01-01.log"))
  file.create(file.path(td, "test.2018-12-31.log"))
  expect_identical(bq$n_backups, 4L)
  backup_date(tf, n_backups = "1 year")
  expect_identical(bq$n_backups, 4L)
  expect_identical(bq$last_backup, as.Date("2019-03-02"))
  expect_identical(as.character(min(bq$backups$timestamp)), "2019-01-01")

  BackupQueue$new(tf)$prune(0)
  file.remove(tf)
})




test_that("backup_date works as expected for years", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  # no backup younger than 1 year exists, so rotate
  bu <- backup_date(tf, "1 year")
  expect_true(file.size(bu) > 1)

  bq <- BackupQueueDate$new(tf)
  expect_true(bq$has_backups)
  bq$prune(0)

  # ensure backup_date believes it is 2019-01-01
  mockery::stub(backup_date, "Sys.Date", as.Date("2019-01-01"))

  # no backup because last backup is less than a year old
  file.create(file.path(td, "test.2019-12-31.log"))
  bu <- backup_date(tf, "1 year")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # roate because backup is from last year
  file.create(file.path(td, "test.2018-12-31.log"))
  bu <- backup_date(tf, "2 year")  # dont rotate
  expect_identical(bq$n_backups, 1L)
  bu <- backup_date(tf, "1 year")  # rotate
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("backup_date works as expected for quarters", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf)

  # no backup younger than 1 quarter exists, so rotate
  bu <- backup_date(tf, "1 quarter")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # ensure backup_date believes it is 2019-01-01
  mockery::stub(backup_date, "Sys.Date",    as.Date("2019-04-01"))

  # no backup because last backup is less than a quarter old
  file.create(file.path(td, "test.2019-06-21.log"))
  bu <- backup_date(tf, "1 quarter")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 quarter old
  file.create(file.path(td, "test.2019-01-01.log"))
  bu <- backup_date(tf, "2 quarter")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 quarter old
  bu <- backup_date(tf, "1 quarter")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})



test_that("backup_date works as expected for months", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf)

  # no backup younger than 1 month exists, so rotate
  bu <- backup_date(tf, "1 month")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # ensure backup_date believes it is 2019-01-01
  mockery::stub(backup_date, "Sys.Date",    as.Date("2019-05-02"))

  # no backup because last backup is less than a month old
  file.create(file.path(td, "test.2019-05-21.log"))
  bu <- backup_date(tf, "1 month")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 month old
  file.create(file.path(td, "test.2019-04-21.log"))
  bu <- backup_date(tf, "2 month")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 month old
  bu <- backup_date(tf, "1 month")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("backup_date works as expected for weeks", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf)

  # no backup younger than 1 week exists, so rotate
  bu <- backup_date(tf, "1 week")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # ensure backup_date believes it is 2019-01-01
  mockery::stub(backup_date, "Sys.Date",    as.Date("2019-01-30"))  # first of week is 2019-01-28

  # no backup because last backup is less than a week old
  file.create(file.path(td, "test.2019-01-28.log"))
  bu <- backup_date(tf, "1 week")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 week old
  file.create(file.path(td, "test.2019-01-27.log"))
  bu <- backup_date(tf, "2 week")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 week old
  bu <- backup_date(tf, "1 week")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("rotate_date works as expected", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  checksum <- tools::md5sum(tf)

  # ensure backup_date believes it is 2019-01-01
  backup <- rotate_date(tf)
  expect_identical(unname(checksum), unname(tools::md5sum(backup)))
  expect_equal(file.size(tf), 0)

  BackupQueueDate$new(tf)$prune(0)
  file.remove(tf)
})




test_that("parse_interval", {
  expect_identical(parse_interval(9)$unit, "day")
  expect_identical(parse_interval("1 week")$unit, "week")
  expect_identical(parse_interval("2 months")$unit, "month")
  expect_identical(parse_interval("3 quarters")$unit, "quarter")
  expect_identical(parse_interval("4 years")$unit, "year")
})



test_that("is_backup_older_than_interval", {
  # week
  expect_false(
    is_backup_older_than_interval(interval = "1 week", as.Date("2019-04-01"), as.Date("2019-04-07"))  # 2019-W14
  )
  expect_true(
    is_backup_older_than_interval(interval = "1 week", as.Date("2019-04-01"), as.Date("2019-04-08"))  # 2019-W14
  )
  expect_false(
    is_backup_older_than_interval(interval = "6 week", as.Date("2019-04-01"),  as.Date("2019-05-06")) # 2019-W19
  )
  expect_true(
    is_backup_older_than_interval(interval = "5 weeks", as.Date("2019-04-01"),  as.Date("2019-05-06")) # 2019-W19
  )

  # month
  expect_false(
    is_backup_older_than_interval(interval = "1 month", as.Date("2019-04-01"), as.Date("2019-04-30"))  # 2019-W14
  )
  expect_true(
    is_backup_older_than_interval(interval = "1 month", as.Date("2019-04-01"), as.Date("2019-05-01"))  # 2019-W14
  )
  expect_false(
    is_backup_older_than_interval(interval = "6 month", as.Date("2019-04-01"),  as.Date("2019-09-01")) # 2019-W19
  )
  expect_true(
    is_backup_older_than_interval(interval = "5 months", as.Date("2019-04-01"),  as.Date("2019-09-06")) # 2019-W19
  )
})




test_that("dry_run does not modify the file systen", {
  expect_length(list.files(td), 0)
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  mockery::stub(backup_date, "Sys.Date", as.Date("2017-05-01"))
  backup_date(tf)

  file.create(c(
    file.path(td, "test.2017.log"),
    file.path(td, "test.201701.log"),
    file.path(td, "test.20170201.log"),
    file.path(td, "test.2017-03.log"),
    file.path(td, "test.2017-04-01.log")
  ))


  snap <- utils::fileSnapshot(td, md5sum = TRUE)
  mockery::stub(backup_date, "Sys.Date", as.Date("2017-05-02"))
  expect_message(backup_date(tf, dry_run = TRUE), "2017-05-02")
  expect_message(backup_date(tf, dry_run = TRUE), "dry_run")
  expect_snapshot_unchanged(snap)

  expect_message(backup_date(tf, dry_run = TRUE, n_backups = 0), "dry_run")
  expect_message(backup_date(tf, dry_run = TRUE, n_backups = 0), "pruning")
  expect_message(backup_date(tf, dry_run = TRUE, n_backups = 0), "2017-03")
  expect_snapshot_unchanged(snap)

  expect_message(
    backup_date(tf, dry_run = TRUE, n_backups = 0, compression = TRUE),
    "zip"
  )
  expect_snapshot_unchanged(snap)

  BackupQueue$new(tf)$prune(0)
  unlink(tf)
  expect_length(list.files(td), 0)
})
