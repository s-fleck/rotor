context("BackupQueueDate")



dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("BackupQueueDate can find and prune backup trails", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)

  expect_identical(bq$n_backups, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01.log.zip", ".2019-01-02.log.tar.gz", ".2019-01-03.log", ".12.log"))
  file.create(bus)
  expect_identical(bq$backups$sfx, c("2019-01-03", "2019-01-02", "2019-01-01"))

  # backup_matrix stays a matrix even if it has only one row
  bq$prune(1)
  expect_identical(bq$n_backups, 1L)

  bq <- BackupQueue$new(tf)
  bq$prune(0)
  bq$prune(0)
  expect_identical(bq$n_backups, 0L)
  file.remove(tf)
})



test_that("BackupQueueDate works with supported datestamp formats", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  bq <- BackupQueueDate$new(tf)
  expect_identical(bq$n_backups, 0L)
  for (i in 1:10) {
    mockery::stub(bq$push_backup, "Sys.time", as.POSIXct(date + i * 5))
    bq$push_backup()
  }
  bq$prune(5)
  expect_length(bq$backups$path, 5)

  file.create(file.path(td, "test.2019-02-20.log"))
  file.create(file.path(td, "test.2019-04.log"))
  file.create(file.path(td, "test.2019.log"))
  file.create(file.path(td, "test.20200411.log"))
  file.create(file.path(td, "test.201905.log"))
  bq$prune(5)

  expect_length(bq$backups$path, 5)

  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)
  on.exit(file.remove(noback))

  expect_path_equal(
    basename(bq$backups$path),
    c(
      "test.20200411.log",
      "test.201905.log",
      "test.2019-04.log",
      "test.2019-02-20.log",
      "test.2019.log"
    )
  )

  bq$prune(0)
  file.remove(tf)
})



test_that("Prune BackupQueueDate based on date", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-01-02.log.tar.gz",
    ".2019-01-03.log",
    ".2020-01-03.log"
  ))
  file.create(bus)
  bq$prune(as.Date("2019-01-02"))

  expect_path_equal(
    basename(bq$backups$path),
    c(
      "test.2020-01-03.log",
      "test.2019-01-03.log",
      "test.2019-01-02.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on year interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-02-02.log.tar.gz",
    ".2019-03-03.log",
    ".2020-01-03.log",
    ".2021-01-03.log",
    ".2022-01-03.log"
  ))
  file.create(bus)

  bq$prune("2 years")
  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2022-01-03.log",
      "test.2021-01-03.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on month interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-02-02.log.tar.gz",
    ".2019-03-03.log",
    ".2019-04-03.log"
  ))
  file.create(bus)

  bq$prune("2 months")
  expect_path_equal(
    basename(bq$backups$path),
    c(
      "test.2019-04-03.log",
      "test.2019-03-03.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on week interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-15.log",
    ".2019-04-08.log.tar.gz",
    ".2019-04-07.log.zip"
  ))
  file.create(bus)

  bq$prune("2 weeks")
  expect_path_equal(
    basename(bq$backups$path),
    c(
      "test.2019-04-15.log",
      "test.2019-04-08.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on dayss interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-07.log.zip",
    ".2019-04-08.log.tar.gz",
    ".2019-04-09.log"
  ))
  file.create(bus)

  bq$prune("2 days")
  expect_path_equal(
    basename(bq$backups$path),
    c(
      "test.2019-04-09.log",
      "test.2019-04-08.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("BackupQueueDate $last_date", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)

  expect_identical(bq$n_backups, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01.log.zip", ".2019-01-02.log.tar.gz", ".2019-01-03.log"))
  file.create(bus)
  expect_identical(bq$backups$sfx, c("2019-01-03", "2019-01-02", "2019-01-01"))

  expect_equal(bq$last_backup, as.Date("2019-01-03"))

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


  bt <- BackupQueueDate$new(tf, backup_dir = bu_dir)
  bt$push_backup()

  expect_match(bt$backups$dir, "rotor.backups")
  bt$push_backup(compression = TRUE)

  expect_identical(bt$n_backups, 2L)
  expect_length(bt$prune(0)$backups$path, 0)
  expect_length(list.files(bu_dir), 0)
})




test_that("parse_date works as expected", {
  expect_equal(parse_date("2018-12-01"), as.Date("2018-12-01"))
  expect_equal(parse_date("20181201"), as.Date("2018-12-01"))
  expect_equal(parse_date("2018-02"), as.Date("2018-02-01"))
  expect_equal(parse_date("201802"), as.Date("2018-02-01"))
  expect_equal(parse_date("2018"), as.Date("2018-01-01"))

  expect_equal(
    parse_date(c("2018-12-02", "20181201", "2018")),
    as.Date(c("2018-12-02", "2018-12-01", "2018-01-01"))
  )

  d  <- as.Date("2019-04-12")
  dt <- as.POSIXct("2019-04-12 23:59:01")
  expect_identical(parse_date(d), d)
  expect_identical(parse_date(dt), d)

  expect_equal(parse_date("2019-04-12"), d)
  expect_equal(parse_date("2019-04"), as.Date("2019-04-01"))
  expect_equal(parse_date("2019"), as.Date("2019-01-01"))

  expect_equal(parse_date("20190412"), d)
  expect_equal(parse_date("201904"), as.Date("2019-04-01"))
  expect_equal(parse_date("2019"), as.Date("2019-01-01"))

})
