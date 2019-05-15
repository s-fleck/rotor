context("BackupQueueDateTime")



dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("is_parsable_date works as expected", {
  expect_true(is_parsable_datetime("2018-12-01"))
  expect_true(is_parsable_datetime("20181201"))
  expect_true(is_parsable_datetime("2018-02"))
  expect_true(is_parsable_datetime("201802"))
  expect_true(is_parsable_datetime("2018"))

  expect_true(is_parsable_datetime(20181231))
  expect_false(is_parsable_datetime(20181232))
  expect_false(is_parsable_datetime("1 week"))
  expect_false(is_parsable_datetime("2 years"))
})




test_that("parse_datetime works as expected", {
  d <- as.Date("2019-12-01")
  expect_equal(parse_datetime(d), as.POSIXct(as.character(d)))

  expect_equal(parse_datetime("2018-12-01"), as.POSIXct("2018-12-01"))
  expect_equal(parse_datetime("20181201"), as.POSIXct("2018-12-01"))
  expect_equal(parse_datetime("2018-02"), as.POSIXct("2018-02-01"))
  expect_equal(parse_datetime("201802"), as.POSIXct("2018-02-01"))
  expect_equal(parse_datetime("2018"), as.POSIXct("2018-01-01"))

  expect_equal(
    parse_datetime(c("2018-12-02", "20181201", "2018")),
    as.POSIXct(c("2018-12-02", "2018-12-01", "2018-01-01"))
  )

  d1 <- as.POSIXct("2019-04-12 17:49:19")
  d2 <- as.POSIXct("2019-04-12 17:49:00")
  d3 <- as.POSIXct("2019-04-12 17:00:00")

  expect_identical(parse_datetime(d1), d1)

  expect_equal(parse_datetime("2019-04-12--17-49-19"), d1)
  expect_equal(parse_datetime("2019-04-12--17-49"), d2)
  expect_equal(parse_datetime("2019-04-12----17"), d3)

  expect_equal(parse_datetime("2019-04-12T17-49-19"), d1)
  expect_equal(parse_datetime("2019-04-12T17-49"), d2)
  expect_equal(parse_datetime("2019-04-12T17"), d3)

  expect_equal(parse_datetime("2019-04-12T174919"), d1)
  expect_equal(parse_datetime("2019-04-12T1749"), d2)
  expect_equal(parse_datetime("2019-04-12T17"), d3)

  expect_equal(parse_datetime("20190412T174919"), d1)
  expect_equal(parse_datetime("20190412T1749"), d2)
  expect_equal(parse_datetime("20190412T17"), d3)

  expect_equal(parse_datetime("20190412174919"), d1)
  expect_equal(parse_datetime("201904121749"), d2)
  expect_equal(parse_datetime("2019041217"), d3)

  expect_equal(
    parse_datetime(c("2019-04-12T17-49-19", "20190412T1749", "2019041217")),
    as.POSIXct(c(d1, d2, d3))
  )
})




test_that("BackupQueueDateTime can find and prune backup trails", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf)

  expect_identical(bq$n_backups, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01T22-22-22.log.zip", ".20190102---1212.log.tar.gz", ".20190103174919.log", ".12.log"))
  file.create(bus)
  expect_identical(
    bq$backups$timestamp,
    as.POSIXct(c("2019-01-03 17:49:19", "2019-01-02 12:12:00", "2019-01-01 22:22:22"))
  )

  # backup_matrix stays a matrix even if it has only one row
  bq$prune(1)
  expect_identical(bq$n_backups, 1L)

  bq <- BackupQueue$new(tf)
  bq$prune(0)
  bq$prune(0)
  expect_identical(bq$n_backups, 0L)
  file.remove(tf)
})




test_that("BackupQueueDatetime works with supported timestamp formats", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  datetime <- as.POSIXct("2019-01-01 00:00:00")

  bq <- BackupQueueDateTime$new(tf)
  expect_identical(bq$n_backups, 0L)
  for (i in 1:10) {
    bq$push_backup(now = datetime + i * 5)
  }
  bq$prune(5)
  expect_length(bq$backups$path, 5)

  file.create(file.path(td, "test.2019-02-20.log"))
  file.create(file.path(td, "test.2019-04-20T12-00-00.log"))
  file.create(file.path(td, "test.2019-04-20T120000.log"))
  file.create(file.path(td, "test.2019-04-20T1200.log"))
  file.create(file.path(td, "test.2019-04-20--12.log"))
  file.create(file.path(td, "test.20190420T12.log"))

  bq$prune(7)

  eres <- c(
    "2019-04-20 12:00:00", "2019-04-20 12:00:00", "2019-04-20 12:00:00",
    "2019-04-20 12:00:00", "2019-04-20 12:00:00", "2019-02-20 00:00:00",
    "2019-01-01 00:00:50"
  )

  expect_identical(
    as.character(bq$backups$timestamp),
    eres
  )

  bq$backups$timestamp
  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)
  on.exit(file.remove(noback))

  expect_identical(
    as.character(bq$backups$timestamp),
    eres
  )

  bq$prune(0)
  file.remove(tf)
})



test_that("Prune BackupQueueDate based on date", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-01-02--12-12-12.log.tar.gz",
    ".2019-01-03.log",
    ".2020-01-03.log"
  ))
  file.create(bus)

  bq$prune(as.Date("2019-01-02"))

  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2020-01-03.log",
      "test.2019-01-03.log",
      "test.2019-01-02--12-12-12.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on year interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01--12-12-12.log.zip",
    ".2019-02-02--12-12-12.log.tar.gz",
    ".2019-03-03--12-12-12.log",
    ".2020-01-03--12-12-12.log",
    ".2021-01-03--12-12-12.log",
    ".2022-01-03--12-12-12.log"
  ))
  file.create(bus)

  bq$prune("2 years")
  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2022-01-03--12-12-12.log",
      "test.2021-01-03--12-12-12.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on month interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01--00-00-00.log.zip",
    ".2019-02-02--00-00-00.log.tar.gz",
    ".2019-03-03--00-00-00.log",
    ".2019-04-03--00-00-00.log"
  ))
  file.create(bus)

  bq$prune("2 months")
  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2019-04-03--00-00-00.log",
      "test.2019-03-03--00-00-00.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on week interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-15--00-00-00.log",
    ".2019-04-08--00-00-00.log.tar.gz",
    ".2019-04-07--00-00-00.log.zip"
  ))
  file.create(bus)

  bq$prune("2 weeks")
  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2019-04-15--00-00-00.log",
      "test.2019-04-08--00-00-00.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on dayss interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-07--00-00-00.log.zip",
    ".2019-04-08--00-00-00.log.tar.gz",
    ".2019-04-09--00-00-00.log"
  ))
  file.create(bus)

  bq$prune("2 days")
  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2019-04-09--00-00-00.log",
      "test.2019-04-08--00-00-00.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("BackupQueueDate $last_date", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf)

  expect_identical(bq$n_backups, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01--00-00-00.log.zip", ".2019-01-02--00-00-00.log.tar.gz", ".2019-01-03--00-00-00.log"))
  file.create(bus)
  expect_identical(bq$backups$sfx, c("2019-01-03--00-00-00", "2019-01-02--00-00-00", "2019-01-01--00-00-00"))
  expect_equal(bq$last_backup, as.POSIXct("2019-01-03--00-00-00"))

  bq$prune(0)
  file.remove(tf)
})




test_that("BackupQueueDateTime$push_backup() can push to different directory", {
  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  tf <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  file.create(tf)
  on.exit(unlink(c(bu_dir, tf), recursive = TRUE))


  bt <- BackupQueueDateTime$new(tf, backup_dir = bu_dir)
  bt$push_backup()

  expect_match(bt$backups$dir, "rotor.backups")
  bt$push_backup(compression = TRUE)

  expect_identical(bt$n_backups, 2L)

  expect_length(bt$prune(0)$backups$path, 0)
  expect_length(list.files(bu_dir), 0)
})
