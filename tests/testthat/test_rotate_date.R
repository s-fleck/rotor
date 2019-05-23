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

  bus <- c(
    file.path(td, "test.1.log"),
    file.path(td, "test.2.log"),
    file.path(td, "test.2017.log"),
    file.path(td, "test.201701.log"),
    file.path(td, "test.20170201.log"),
    file.path(td, "test.2017-03.log"),
    file.path(td, "test.2017-04-01.log")
  )
  file.create(c(bus, tf))
  writeLines("test", tf)

  expect_warning(
    bu <- backup_date(tf),
    "test\\.1\\.log.*test\\.2\\.log"
  )
  unlink(bus)
  prune_backups(tf, 0)
  unlink(tf)

  expect_dir_empty(td)
})



test_that("backup/rotate_date works with size", {
  tf     <- file.path(td, "test.log")
  expect_dir_empty(td)
  on.exit(unlink(tf))
  saveRDS(iris, tf)
  size_ori <- file.size(tf)

  rotate_date(tf, size = "5kb")
  expect_identical(n_backups(tf), 0L)
  expect_equal(file.size(tf), size_ori)

  rotate_date(tf, size = "0.5kb")
  expect_identical(n_backups(tf), 1L)
  expect_equal(file.size(tf), 0)

  prune_backups(tf, 0)
  unlink(tf)
  expect_dir_empty(td)
})




test_that("backup/rotate_date fails if backup already exists for that period", {
  tf <- file.path(td, "test.log")
  on.exit(unlink(tf))
  saveRDS(iris, tf)

  now <- Sys.Date()
  backup_date(tf, now = now)
  expect_identical(n_backups(tf), 1L)

  expect_error(backup_date(tf, now = now, age = -1), "exists")
  prune_backups(tf, 0)
  unlink(tf)
  expect_dir_empty(td)
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
  on.exit(unlink(tf))
  writeLines("test", tf)

  backup_date(tf, age = "1 month", now = "2019-02-28")
  expect_identical(n_backups(tf), 1L)

  backup_date(tf, age = "1 month", now = "2019-03-01")
  expect_identical(n_backups(tf), 2L)

  #' When pruning/limiting backup queues, `"1 year"` means "keep at least most
  #' one year worth of backups". So if you call
  #' `backup_date(myfile, max_backups = "1 year")` on `2019-03-01`, it will create
  #' a backup and then remove all backups of `myfile` before `2019-01-01`.
  file.create(file.path(td, "test.2019-01-01.log"))
  file.create(file.path(td, "test.2018-12-31.log"))
  expect_identical(n_backups(tf), 4L)
  backup_date(tf, max_backups = "1 year", now = "2019-03-02")
  expect_identical(n_backups(tf), 4L)
  expect_match(newest_backup(tf), "2019-03-02")
  expect_match(oldest_backup(tf), "2019-01-01")

  prune_backups(tf, 0)
  unlink(tf)
  expect_dir_empty(td)
})




test_that("backup_date works as expected for years", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  snap <- fileSnapshot(td)
  # no backup younger than 1 year exists, so rotate

  # dry run does nothing
  expect_message(bu <- backup_date(tf, "1 year", dry_run = TRUE), "copy")
  expect_snapshot_unchanged(snap)
  bu <- backup_date(tf, "1 year")
  expect_true(file.size(bu) > 1)

  bq <- BackupQueueDate$new(tf, cache_backups = FALSE)
  expect_true(bq$has_backups)
  bq$prune(0)

  # no backup because last backup is less than a year old
  file.create(file.path(td, "test.2019-12-31.log"))
  bu <- backup_date(tf, "1 year", now = "2019-01-01")
  bu <- backup_date(tf, "1 year", now = Sys.time())
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # rotate because backup is from last year
  file.create(file.path(td, "test.2018-12-31.log"))
  bu <- backup_date(tf, "2 year")  # dont rotate
  expect_identical(bq$n_backups, 1L)
  bu <- backup_date(tf, "1 year")  # rotate
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  unlink(tf)
  expect_dir_empty(td)
})




test_that("backup_date works as expected for quarters", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf, cache_backups = FALSE)

  # no backup younger than 1 quarter exists, so rotate
  bu <- backup_date(tf, "1 quarter")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than a quarter old
  file.create(file.path(td, "test.2019-06-21.log"))
  bu <- backup_date(tf, "1 quarter", now = "2019-04-01")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 quarter old
  file.create(file.path(td, "test.2019-01-01.log"))
  bu <- backup_date(tf, "2 quarter", now = "2019-04-01")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 quarter old
  bu <- backup_date(tf, "1 quarter", now = "2019-04-01")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("backup_date works as expected for months", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf, cache_backups = FALSE)

  # no backup younger than 1 month exists, so rotate
  bu <- backup_date(tf, "1 month")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than a month old
  file.create(file.path(td, "test.2019-05-21.log"))
  bu <- backup_date(tf, "1 month", now = "2019-05-02")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 month old
  file.create(file.path(td, "test.2019-04-21.log"))
  bu <- backup_date(tf, "2 month", now = "2019-05-02")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 month old
  bu <- backup_date(tf, "1 month", now = "2019-05-02")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("backup_date works as expected for weeks", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  bq <- BackupQueueDate$new(tf, cache_backups = FALSE)

  # no backup younger than 1 week exists, so rotate
  bu <- backup_date(tf, "1 week")
  expect_true(file.size(bu) > 1)
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)


  # no backup because last backup is less than a week old
  file.create(file.path(td, "test.2019-01-28.log"))
  bu <- backup_date(tf, "1 week", now = "2019-01-30")
  expect_identical(bq$n_backups, 1L)
  bq$prune(0)

  # no backup because last backup is less than 2 week old
  file.create(file.path(td, "test.2019-01-27.log"))
  bu <- backup_date(tf, "2 week", now = "2019-01-30")
  expect_identical(bq$n_backups, 1L)

  # backup because last backup is more than 1 week old
  bu <- backup_date(tf, "1 week", now = "2019-01-30")
  expect_true(length(bq$backups$path) == 2)

  bq$prune(0)
  file.remove(tf)
})




test_that("backup_date works as expected for Inf", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  on.exit({
    prune_backups(tf, 0)
    unlink(tf)
  })

  # no backup younger than 1 week exists, so rotate
  bu <- backup_date(tf, Inf)
  expect_true(file.size(bu) > 0)
  expect_identical(n_backups(tf), 0L)
  prune_backups(tf, 0)


  # no backup because last backup is less than a week old
  backup_date(tf, -1, now = "2019-01-28")
  expect_identical(n_backups(tf), 1L)
  backup_date(tf, Inf, now = "2999-01-30")
  expect_identical(n_backups(tf), 1L)
})




test_that("rotate_date works as expected", {
  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  checksum <- tools::md5sum(tf)

  # ensure backup_date believes it is 2019-01-01
  rotate_date(tf)
  expect_identical(unname(checksum), unname(tools::md5sum(newest_backup(tf))))
  expect_equal(file.size(tf), 0)

  BackupQueueDate$new(tf)$prune(0)
  file.remove(tf)
})




test_that("dry_run does not modify the file systen", {
  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  expect_length(list.files(td), 0)
  tf <- file.path(td, "test.log")

  saveRDS(iris, tf)
  backup_date(tf, now = "2017-05-01")
  file.create(c(
    file.path(td, "test.2017.log"),
    file.path(td, "test.201701.log"),
    file.path(td, "test.20170201.log"),
    file.path(td, "test.2017-03.log"),
    file.path(td, "test.2017-04-01.log")
  ))


  snap <- utils::fileSnapshot(td, md5sum = TRUE)
  expect_silent({
    expect_message(backup_date(tf, dry_run = TRUE, now = "2017-05-02"), "copying")
    expect_snapshot_unchanged(snap)
    expect_message(backup_date(tf, dry_run = TRUE, max_backups = 0), "dry_run")
    expect_message(backup_date(tf, dry_run = TRUE, max_backups = 0), "removing")
    expect_message(backup_date(tf, dry_run = TRUE, max_backups = 0), "2017-03")
  })

  expect_snapshot_unchanged(snap)

  expect_message(
    backup_date(tf, dry_run = TRUE, max_backups = 0, compression = TRUE),
    "zip"
  )
  expect_snapshot_unchanged(snap)

  BackupQueue$new(tf)$prune(0)
  unlink(tf)
  expect_length(list.files(td), 0)
})




test_that("backup/rotate_time works to different directory", {
  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  on.exit(unlink(c(tf, bu_dir), recursive = TRUE))

  file.create(tf)
  writeLines("foobar", tf)

  backup_time(tf, backup_dir = bu_dir, now = as.Date("2019-01-01"))

  expect_identical(
    readLines(tf),
    readLines(file.path(dirname(tf), "backups", "test.2019-01-01--00-00-00.log"))
  )

  expect_identical(n_backups(tf, backup_dir = bu_dir), 1L)
  prune_backups(tf, 0, backup_dir = bu_dir)
  expect_identical(n_backups(tf, backup_dir = bu_dir), 0L)
  expect_length(list.files(bu_dir), 0)
})




test_that("backup/rotate_time works with custom format", {
  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  on.exit(unlink(c(tf, bu_dir), recursive = TRUE))

  file.create(tf)
  writeLines("foobar", tf)

  backup_time(tf, backup_dir = bu_dir, now = as.Date("2019-01-01"), format = "%Y-%m")

  expect_identical(
    readLines(tf),
    readLines(file.path(dirname(tf), "backups", "test.2019-01.log"))
  )

  expect_identical(n_backups(tf, backup_dir = bu_dir), 1L)
  prune_backups(tf, 0, backup_dir = bu_dir)
  expect_identical(n_backups(tf, backup_dir = bu_dir), 0L)
  expect_length(list.files(bu_dir), 0)
})

