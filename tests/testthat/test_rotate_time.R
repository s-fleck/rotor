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





# legacy tests ------------------------------------------------------------




test_that("roate_date keeps expected number of backups", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  for (i in 1:10) {
    backup_date(
      tf,
      n_backups = 5,
      date = date + i * 5
    )
  }

  res <- find_backups(tf)

  expect_length(res, 5)

  expect_identical(
    basename(res),
    c("test.2019-01-31.log",
      "test.2019-02-05.log",
      "test.2019-02-10.log",
      "test.2019-02-15.log",
      "test.2019-02-20.log"
    )
  )

  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})



test_that("compressed roate_date works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  for (i in 1:10) {
    backup_date(
      tf,
      n_backups = 5,
      date = date + i * 5
    )
  }
  expect_length(find_backups(tf), 5)

  for (i in 1:7) {
    backup_date(
      tf,
      n_backups = 10,
      date = date + 60 + i * 5,
      compression = "zip"
    )
  }
  r <- find_backups(tf)
  expect_length(r, 10)
  expect_identical(
    tools::file_ext(find_backups(tf)),
    c(rep("log", 3), rep("zip", 7))
  )

  expect_identical(last(basename(r)), "test.2019-04-06.log.zip")
  expect_identical(first(basename(r)), "test.2019-02-10.log")


  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})




test_that("parse_date works as expected", {
  expect_identical(parse_date("2018-12-01"), as.Date("2018-12-01"))
  expect_identical(parse_date("20181201"), as.Date("2018-12-01"))
  expect_identical(parse_date("2018-02"), as.Date("2018-02-01"))
  expect_identical(parse_date("201802"), as.Date("2018-02-01"))
  expect_identical(parse_date("2018"), as.Date("2018-01-01"))
})



test_that("is_parsable_date works as expected", {
  expect_true(is_parsable_date("2018-12-01"))
  expect_true(is_parsable_date("20181201"))
  expect_true(is_parsable_date("2018-02"))
  expect_true(is_parsable_date("201802"))
  expect_true(is_parsable_date("2018"))

  expect_true(is_parsable_date(20181231))
  expect_false(is_parsable_date(20181232))
  expect_false(is_parsable_date("1 week"))
  expect_false(is_parsable_date("2 years"))
})




test_that("backup_time works", {
  skip("needs rewrite")
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  bq <- BackupQueueDate$new(tf)
  expect_identical(bq$n_backups, 0L)

  mockery::stub(bq$push_backup, "Sys.Date", date + i * 5)
  backup_time(tf, date = date  + 100, format = "%Y%m%d")
  backup_time(tf, date = date  + 110, format = "%Y-%m")
  backup_time(tf, date = date  + 120, format = "%Y%m")
  backup_time(tf, date = date  + 130, format = "%Y", n_backups = 5)
  expect_length(bq$backups$path, 5)

  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)
  on.exit(file.remove(noback))

  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2019-02-20.log",
      "test.2019-04.log",
      "test.2019.log",
      "test.20190411.log",
      "test.201905.log"
    )
  )

  bq$prune(0)
  file.remove(tf)
})

