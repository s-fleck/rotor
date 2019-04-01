context("BackupTrail")


dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("BackupTrail works as expected", {
  tf <- file.path(td, "test")
  file.create(tf)

  bt <- BackupTrailIndexed$new(tf)
  expect_identical(bt$file, tf)
  expect_identical(bt$backup_dir, dirname(tf))
  file.remove(tf)
})




test_that("BackupTrail works as expected for files without extension", {
  tf <- file.path(td, "test")
  file.create(tf)
  bt <- BackupTrailIndexed$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tf, c(".1", ".2", ".3"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  bt$prune(2)
  expect_identical(bt$backups, bus[1:2])
  bt$prune(0)
  expect_identical(bt$backups, character(0))
  file.remove(tf)
})




test_that("BackupTrail works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupTrailIndexed$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log", ".2.log", ".3.log"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  bt$prune(2)
  expect_identical(bt$backups, bus[1:2])
  bt$prune(0)
  expect_identical(bt$backups, character(0))
  file.remove(tf)
})




test_that("BackupTrailDate works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  bt <- BackupTrailDate$new(tf)
  expect_identical(bt$backups, character(0))
  for (i in 1:10) {
    backup_date(
      tf,
      max_backups = 5,
      date = date + i * 5
    )
  }
  expect_length(bt$backups, 5)

  backup_date(tf, date = date  + 100, format = "%Y%m%d")
  backup_date(tf, date = date  + 110, format = "%Y-%m")
  backup_date(tf, date = date  + 120, format = "%Y%m")
  backup_date(tf, date = date  + 130, format = "%Y", max_backups = 5)
  expect_length(bt$backups, 5)

  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)
  on.exit(file.remove(noback))

  expect_identical(
    bt$backups,
    c(
      "/tmp/RtmplsgPcI/rotor/test.2019-02-20.log",
      "/tmp/RtmplsgPcI/rotor/test.2019-04.log",
      "/tmp/RtmplsgPcI/rotor/test.2019.log",
      "/tmp/RtmplsgPcI/rotor/test.20190411.log",
      "/tmp/RtmplsgPcI/rotor/test.201905.log"
    )
  )
})
