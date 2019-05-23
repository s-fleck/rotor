context("utils-predicates")




test_that("is_zipcmd_available detects zipcommand", {
  # can only return true on platforms with a zip command
  skip_if_not(is_zipcmd_available(), "No zipcommand found")
  expect_true(is_zipcmd_available())
})




test_that("is_zipcmd_available() detects missing zipcommand", {
  expect_false(is_zipcmd_available("sdjkghsaghaskjghsagj"))
})




test_that("utils-fs can create/remove files in dry_run memory", {
  td <- file.path(tempdir(), "rotor")
  tf1 <- file.path(td, "foo")
  tf2 <- file.path(td, "bar")
  dir.create(tf1, recursive = TRUE)
  file.create(tf2)

  on.exit(unlink(c(tf2, tf1, td), recursive = TRUE))

  expect_true(is_dir(tf1))
  expect_false(is_dir(tf2))
})



test_that("is_pure_BackupQueue", {
  td  <- file.path(tempdir(), "rotor")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))
  tf <- file.path(td, "test.log")
  file.create(tf)

  # Empty Queue
  expect_true(is_pure_BackupQueue(tf))
  expect_true(is_pure_BackupQueueDateTime(tf))
  expect_true(is_pure_BackupQueueIndex(tf))

  # With a Date Backup
  tf_date <- file.path(td, "test.2017.log")
  file.create(tf_date)
  expect_true(is_pure_BackupQueue(tf))
  expect_true(is_pure_BackupQueueDateTime(tf))
  expect_false(is_pure_BackupQueueIndex(tf))


  # With mixed backups
  tf_idx <- file.path(td, "test.1.log")
  file.create(tf_idx)
  expect_false(is_pure_BackupQueue(tf))
  expect_false(is_pure_BackupQueueDateTime(tf))
  expect_false(is_pure_BackupQueueIndex(tf))

  expect_error(assert_pure_BackupQueue(tf), "not possible")
  expect_warning(assert_pure_BackupQueue(tf, warn_only = TRUE))

  # With indexed backups
  file.remove(tf_date)
  expect_true(is_pure_BackupQueue(tf))
  expect_false(is_pure_BackupQueueDateTime(tf))
  expect_true(is_pure_BackupQueueIndex(tf))

  unlink(tf_idx)
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




test_that("is_backup_older_than_interval works as expected", {
  now <- as.POSIXct("2019-12-11 00:12:13")

  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), "0 days", now))
  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), "-1 days", now))
  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), 0, now))
  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), -1, now))

  expect_false(is_backup_older_than_interval(as.Date("2019-01-01"), "1 year", now))
  expect_true(is_backup_older_than_interval(as.Date("2018-12-12"), "1 year", now))
  expect_true(is_backup_older_than_interval(as.Date("2018-12-12"), "0 year", now))
})




test_that("is_backup_older_than_datetime works as expected", {
  now <- as.POSIXct("2019-12-11 00:12:13")

  expect_true(is_backup_older_than_datetime(as.Date("2019-12-11"), now))
  expect_false(is_backup_older_than_datetime(as.Date("2019-12-11"), as.Date(now)))
  expect_false(is_backup_older_than_datetime(as.Date("2019-12-12"), now))
})
