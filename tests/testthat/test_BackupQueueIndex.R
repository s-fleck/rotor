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
  expect_identical(bt$n_backups, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log.zip", ".2.log.tar.gz", ".3.log"))
  file.create(bus)
  expect_path_equal(bt$backups$path, bus)

  # multiple pruning with the same settings does not change anything
  expect_path_equal(bt$prune(2)$backups$path, bus[1:2])
  expect_path_equal(bt$prune(2)$backups$path, bus[1:2])
  expect_path_equal(bt$prune(2)$backups$path, bus[1:2])

  # pruning with higher prune number than number of backups does not change anything
  expect_path_equal(bt$prune(1)$backups$path, bus[1])
  expect_path_equal(bt$prune(2)$backups$path, bus[1])

  #cleanup
  expect_length(bt$prune(0)$backups$path, 0)
  expect_length(bt$prune(0)$backups$path, 0)
})




test_that("BackupQueueIndex only shows indexed backups", {
  tf <- file.path(td, "test.log")

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

  bq <- BackupQueueIndex$new(tf)
  expect_true(all(bq$backups$sfx == as.integer(bq$backups$sfx)))

  BackupQueue$new(tf)$prune(0)
  file.remove(tf)
})



test_that("BackupQueue pruning works as expected for files without extension", {
  tf <- file.path(td, "test")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$n_backups, 0L)
  bus <- paste0(tf, c(".1", ".2", ".3"))
  file.create(bus)
  expect_path_equal(bt$backups$path, bus)
  bt$prune(2)
  expect_path_equal(bt$backups$path, bus[1:2])
  bt$prune(2)
  expect_path_equal(bt$backups$path, bus[1:2])
  expect_length(bt$prune(0)$backups$path, 0)
  file.remove(tf)
})




test_that("BackupQueue works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$n_backups, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log", ".2.log", ".3.log"))
  file.create(bus)
  expect_path_equal(bt$backups$path, bus)
  bt$prune(2)
  expect_path_equal(bt$backups$path, bus[1:2])
  expect_length(bt$prune(0)$backups$path, 0)
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
  expect_path_setequal(bt$backups$path, bus)
  expect_path_setequal(bt$pad_index()$backups$path, padded_bus)
  expect_path_setequal(bt$prune(9)$backups$path, bus[1:9])

  expect_length(bt$prune(0)$backups$path, 0)
  file.remove(tf)
})



test_that("BackupQueue$increment_index works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:9, ".log")
  pushed_bus <- paste0(tools::file_path_sans_ext(tf), ".", pad_left(2:10, pad = "0"), ".log")
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  expect_path_setequal(bt$backups$path, bus)
  expect_path_equal(bt$increment_index()$backups$path, pushed_bus)

  expect_length(bt$prune(0)$backups$path, 0)
  file.remove(tf)
})




test_that("BackupQueue$push_backup() works as expected", {
  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:9, ".log")
  padded_bus <- sort(paste0(
    tools::file_path_sans_ext(tf), ".", pad_left(1:10, pad = 0), ".log"
  ))
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  bt$push_backup()
  expect_length(bt$backups$path, 10)

  bt$push_backup(compression = TRUE)
  expect_length(bt$backups$path, 11)
  expect_identical(tools::file_ext(bt$backups$path[[1]]), "zip")
  expect_setequal(tools::file_ext(bt$backups$path[2:11]), "log")

  expect_length(bt$prune(0)$backups$path, 0)
  file.remove(tf)
})




test_that("BackupQueueIndex$push_backup() can push to different directory", {
  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  tf <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  file.create(tf)
  on.exit(unlink(c(bu_dir, tf), recursive = TRUE))


  bt <- BackupQueueIndex$new(tf, backup_dir = bu_dir)
  bt$push_backup()

  expect_match(bt$backups$dir, "rotor.backups")
  bt$push_backup(compression = TRUE)

  expect_identical(bt$n_backups, 2L)

  expect_length(bt$prune(0)$backups$path, 0)
  expect_length(list.files(bu_dir), 0)
})




test_that("BackupQueueIndex dry run doesnt modify file system", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log.zip", ".2.log.tar.gz", ".3.log"))
  file.create(bus)

  DRY_RUN$activate()
  on.exit({
    file.remove(tf)
    DRY_RUN$deactivate()
  })

  snap <- utils::fileSnapshot(td, md5sum = TRUE)

  expect_message(bt$increment_index(92), "93")
  expect_snapshot_unchanged(snap)

  expect_silent(bt$pad_index())
  expect_snapshot_unchanged(snap)

  expect_message(bt$push_backup(), "test.log -> test.1.log")
  expect_snapshot_unchanged(snap)

  expect_message(bt$prune(0), "test.01.log")
  expect_snapshot_unchanged(snap)
})
