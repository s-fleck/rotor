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
    mockery::stub(bq$push_backup, "Sys.Date", date + i * 5)
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

  expect_identical(
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

  expect_identical(
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
  expect_identical(
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
  expect_identical(
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
  expect_identical(
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
  bq$backups

  expect_identical(bq$n_backups, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01.log.zip", ".2019-01-02.log.tar.gz", ".2019-01-03.log"))
  file.create(bus)
  expect_identical(bq$backups$sfx, c("2019-01-03", "2019-01-02", "2019-01-01"))

  expect_identical(bq$last_backup, as.Date("2019-01-03"))

  bq$prune(0)
  file.remove(tf)
})
