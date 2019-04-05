context("rotate_interval")


dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})


test_that("rotate_interval works as expected for years", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  # no backup younger than 1 year exists, so rotate
  bu <- rotate_interval(tf, "1 year")
  expect_true(file.size(bu) > 1)

  bq <- BackupQueueDate$new(tf)
  expect_true(bq$has_backups)
  bq$prune(0)

  # ensure rotate_interval believes it is 2019-01-01
  mockery::stub(rotate_interval, "Sys.Date",    as.Date("2019-01-01"))

  # no backup because last backup is less than a year old
  file.create(file.path(td, "test.2019-12-31.log"))
  bu <- rotate_interval(tf, "1 year")
  expect_true(length(bq$backups) == 1)
  bq$prune(0)

  # roate because backup is from last year
  file.create(file.path(td, "test.2018-12-31.log"))
  bu <- rotate_interval(tf, "2 year")  #dont rotate
  expect_true(length(bq$backups) == 1)
  bu <- rotate_interval(tf, "1 year")  #rotate
  expect_true(length(bq$backups) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("rotate_interval works as expected for quarters", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf)

  # no backup younger than 1 quarter exists, so rotate
  bu <- rotate_interval(tf, "1 quarter")
  expect_true(file.size(bu) > 1)
  expect_true(length(bq$backups) == 1)
  bq$prune(0)

  # ensure rotate_interval believes it is 2019-01-01
  mockery::stub(rotate_interval, "Sys.Date",    as.Date("2019-04-01"))
  mockery::stub(rotate_interval, "Sys.date_yq", dint::date_yq(2019, 2))


  # no backup because last backup is less than a quarter old
  file.create(file.path(td, "test.2019-06-21.log"))
  bu <- rotate_interval(tf, "1 quarter")
  expect_true(length(bq$backups) == 1)
  bq$prune(0)

  # no backup because last backup is less than 1 quarter old
  file.create(file.path(td, "test.2019-01-01.log"))
  bu <- rotate_interval(tf, "2 quarter")
  expect_true(length(bq$backups) == 1)

  # backup because last backup is more than 1 quarter old
  bu <- rotate_interval(tf, "1 quarter")
  expect_true(length(bq$backups) == 2)

  bq$prune(0)
  file.remove(tf)
})



test_that("rotate_interval works as expected for months", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  # no backup younger than 1 month exists, so rotate
  bu <- rotate_interval(tf, "1 month")
  expect_true(file.size(bu) > 1)

  bq <- BackupQueueDate$new(tf)
  expect_true(bq$has_backups)

  mockery::stub(rotate_interval, "Sys.Date",    as.Date("2019-04-01"))
  mockery::stub(rotate_interval, "Sys.date_ym", dint::date_yq(2019, 2))

  # no backup because last backup is less than a month old
  bu <- rotate_interval(tf, "1 month")
  expect_true(length(bq$backups) == 1)

  # no backup because last backup is less than 2 months old
  replace_date_stamp(bq$backups, -31)
  bu <- rotate_interval(tf, "2 month")
  expect_true(length(bq$backups) == 1)

  # backup because last backup is more than 1 month old
  bu <- rotate_interval(tf, "1 month")
  expect_true(length(bq$backups) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("rotate_interval works as expected", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  # no backup younger than 1 week exists, so rotate
  bu <- rotate_interval(tf, "1 week")
  expect_true(file.size(bu) > 1)

  bq <- BackupQueueDate$new(tf)
  expect_true(bq$has_backups)

  mockery::stub(rotate_interval, "Sys.Date",    as.Date("2019-04-01"))
  mockery::stub(rotate_interval, "Sys.date_yw", dint::date_yw(2019, 1))
  # no backup because last backup is less than a week old
  bu <- rotate_interval(tf, "1 week")
  expect_true(length(bq$backups) == 1)

  # no backup because last backup is less than 2 weeks old
  replace_date_stamp(bq$backups, -7)
  bu <- rotate_interval(tf, "2 week")
  expect_true(length(bq$backups) == 1)

  # backup because last backup is more than 1 week old
  bu <- rotate_interval(tf, "1 week")
  expect_true(length(bq$backups) == 2)

  expect_true(length(bq$backups) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("rotate_interval works as expected", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  rotate_size(tf, 1)
  expect_warning(rotate_interval(tf, "1 week"))

  bq <- BackupQueueDate$new(tf)
  expect_length(bq$backups, 1)

  bq <- BackupQueueIndex$new(tf)
  expect_length(bq$backups, 1)

  bq <- BackupQueue$new(tf)
  expect_length(bq$backups, 2)

  expect_length(bq$prune(0)$backups, 0)
  rm(tf)
})




test_that("parse interval", {
  expect_identical(parse_interval(9)$unit, "day")
  expect_identical(parse_interval("1 week")$unit, "week")
  expect_identical(parse_interval("2 months")$unit, "month")
  expect_identical(parse_interval("3 quarters")$unit, "quarter")
  expect_identical(parse_interval("4 years")$unit, "year")
})
