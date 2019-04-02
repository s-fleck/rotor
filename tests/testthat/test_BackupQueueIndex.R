context("BackupQueueIndex")




dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("BackupQueueIndex can find and prune backup trails", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log.zip", ".2.log.tar.gz", ".3.log"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  bt

  # multiple pruning with the same settings does not change anything
  expect_identical(bt$prune(2)$backups, bus[1:2])
  expect_identical(bt$prune(2)$backups, bus[1:2])
  expect_identical(bt$prune(2)$backups, bus[1:2])

  # pruning with higher prune number than number of backups does not change anything
  expect_identical(bt$prune(1)$backups, bus[1])
  expect_identical(bt$prune(2)$backups, bus[1])

  #cleanup
  expect_length(bt$prune(0)$backups, 0)
  expect_length(bt$prune(0)$backups, 0)
})




test_that("BackupQueue pruning works as expected for files without extension", {
  tf <- file.path(td, "test")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tf, c(".1", ".2", ".3"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  bt$prune(2)
  expect_identical(bt$backups, bus[1:2])
  bt$prune(2)
  expect_identical(bt$backups, bus[1:2])
  expect_length(bt$prune(0)$backups, 0)
  file.remove(tf)
})




test_that("BackupQueue works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$backups, character(0))
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log", ".2.log", ".3.log"))
  file.create(bus)
  expect_identical(bt$backups, bus)
  bt$prune(2)
  expect_identical(bt$backups, bus[1:2])
  expect_length(bt$prune(0)$backups, 0)
  file.remove(tf)
})




test_that("BackupQueue$pad_index works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:12, ".log")
  padded_bus <- sort(paste0(
    tools::file_path_sans_ext(tf), ".", pad_left(1:12, pad = 0), ".log"
  ))
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  expect_setequal(bt$backups, bus)
  expect_identical(bt$pad_index()$backups, padded_bus)
  expect_identical(bt$prune(9)$backups, bus[1:9])

  expect_length(bt$prune(0)$backups, 0)
  file.remove(tf)
})



test_that("BackupQueue$increment_index works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:9, ".log")
  pushed_bus <- paste0(tools::file_path_sans_ext(tf), ".", pad_left(2:10, pad = "0"), ".log")
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  expect_setequal(bt$backups, bus)
  expect_identical(bt$increment_index()$backups, pushed_bus)

  expect_length(bt$prune(0)$backups, 0)
  file.remove(tf)
})




test_that("BackupQueue$push_backup() works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:9, ".log")
  padded_bus <- sort(paste0(
    tools::file_path_sans_ext(tf), ".", pad_left(1:10, pad = 0), ".log"
  ))
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  bt$push_backup()
  expect_length(bt$backups, 10)

  bt$push_backup(compression = TRUE)
  expect_length(bt$backups, 11)
  expect_identical(tools::file_ext(bt$backups[[1]]), "zip")
  expect_setequal(tools::file_ext(bt$backups[2:11]), "log")

  expect_length(bt$prune(0)$backups, 0)
  file.remove(tf)
})
