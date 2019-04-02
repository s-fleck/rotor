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
  bt <- BackupQueueDate$new(tf)

  expect_identical(bt$backups, character(0))
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01.log.zip", ".2019-01-02.log.tar.gz", ".2019-01-03.log"))
  file.create(bus)
  expect_identical(bt$backup_matrix[, "sfx"], c("2019-01-03", "2019-01-02", "2019-01-01"))
  bt$prune(0)
  file.remove(tf)
})

test_that("BackupQueueDate works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  bt <- BackupQueueDate$new(tf)
  expect_identical(bt$backups, character(0))
  for (i in 1:10) {
    backup_date(
      tf,
      n_backups = 5,
      date = date + i * 5
    )
  }
  expect_length(bt$backups, 5)

  backup_date(tf, date = date  + 100, format = "%Y%m%d")
  backup_date(tf, date = date  + 110, format = "%Y-%m")
  backup_date(tf, date = date  + 120, format = "%Y%m")
  backup_date(tf, date = date  + 130, format = "%Y", n_backups = 5)
  expect_length(bt$backups, 5)

  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)
  on.exit(file.remove(noback))

  expect_identical(
    basename(bt$backups),
    c(
      "test.2019-02-20.log",
      "test.2019-04.log",
      "test.2019.log",
      "test.20190411.log",
      "test.201905.log"
    )
  )

  bt$prune(0)
  file.remove(tf)
})






test_that("Prune BackupQueueDate based on date", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-01-02.log.tar.gz",
    ".2019-01-03.log",
    ".2020-01-03.log"
  ))
  file.create(bus)
  bt$prune(as.Date("2019-01-02"))

  expect_identical(
    basename(bt$backups),
    c(
      "test.2019-01-02.log.tar.gz",
      "test.2019-01-03.log",
      "test.2020-01-03.log"
    )
  )
  bt$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on year interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-02-02.log.tar.gz",
    ".2019-03-03.log",
    ".2020-01-03.log",
    ".2021-01-03.log",
    ".2022-01-03.log"
  ))
  file.create(bus)

  bt$prune("2 years")
  expect_identical(
    basename(bt$backups),
    c(
      "test.2021-01-03.log",
      "test.2022-01-03.log"
    )
  )
  bt$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on month interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-02-02.log.tar.gz",
    ".2019-03-03.log",
    ".2019-04-03.log"
  ))
  file.create(bus)

  bt$prune("2 months")
  expect_identical(
    basename(bt$backups),
    c(
      "test.2019-03-03.log",
      "test.2019-04-03.log")
  )
  bt$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on week interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-07.log.zip",
    ".2019-04-08.log.tar.gz",
    ".2019-04-15.log"
  ))
  file.create(bus)

  bt$prune("2 weeks")
  expect_identical(
    basename(bt$backups),
    c(
      "test.2019-04-08.log.tar.gz",
      "test.2019-04-15.log"
    )
  )
  bt$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on week interval", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueDate$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-07.log.zip",
    ".2019-04-08.log.tar.gz",
    ".2019-04-09.log"
  ))
  file.create(bus)

  bt$prune("2 days")
  expect_identical(
    basename(bt$backups),
    c(
      "test.2019-04-08.log.tar.gz",
      "test.2019-04-09.log"
    )
  )
  bt$prune(0)
  file.remove(tf)
})
