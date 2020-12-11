context("utils-predicates")




test_that("is_zipcmd_available detects zipcommand", {
  # can only return true on platforms with a zip command
  skip_if_not(is_zipcmd_available(), "system zip-command is available")
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




test_that("is_parsable_date/time works", {

  expect_true(is_parsable_datetime(as.Date("2018-12-01")))
  expect_true(is_parsable_datetime(as.POSIXct("2018-12-01 12:01:01")))

  expect_true(is_parsable_datetime("2018-12-01"))
  expect_true(is_parsable_datetime("20181201"))
  expect_true(is_parsable_datetime("2018-02"))
  expect_true(is_parsable_datetime("201802"))
  expect_true(is_parsable_datetime("2018"))

  expect_true(is_parsable_datetime(20181231))
  #expect_false(is_parsable_datetime(20181232))
  expect_false(is_parsable_datetime("1 week"))
  expect_false(is_parsable_datetime("2 years"))

  expect_true(is_parsable_datetime("2019-12-12--13-12-11"))
  expect_true(is_parsable_datetime("2019-12-12--13-12-"))
  expect_true(is_parsable_datetime("2019-12-12--13--"))
  expect_true(is_parsable_datetime("2019-12-12----"))
  expect_true(is_parsable_datetime("2019-12-----"))
  expect_true(is_parsable_datetime("2019------"))
  expect_false(is_parsable_datetime("------"))


  expect_true(is_parsable_datetime("2019-12-12T13-12-11"))
  expect_true(is_parsable_datetime("2019-12-12T13-12-"))
  expect_true(is_parsable_datetime("2019-12-12T13T"))
  expect_true(is_parsable_datetime("2019-12-12TT"))
  expect_true(is_parsable_datetime("2019-12TT-"))
  expect_true(is_parsable_datetime("2019TTT"))
  expect_false(is_parsable_datetime("TTT"))

  expect_true(is_parsable_datetime("2019-12-12 13-12-11"))
  expect_true(is_parsable_datetime("2019-12-12 13-12-"))
  expect_true(is_parsable_datetime("2019-12-12 13 "))
  expect_true(is_parsable_datetime("2019-12-12  "))
  expect_true(is_parsable_datetime("2019-12  -"))
  expect_true(is_parsable_datetime("2019   "))
  expect_false(is_parsable_datetime("   "))

  expect_false(is_parsable_date("2019-12-12--13-12-11"))
  expect_false(is_parsable_date("2019-12-12--13-12-"))
  expect_false(is_parsable_date("2019-12-12--13--"))
  expect_true(is_parsable_date("2019-12-12----"))
  expect_true(is_parsable_date("2019-12-----"))
  expect_true(is_parsable_date("2019------"))
  expect_false(is_parsable_date("------"))
})





test_that("is_backup_older_than_interval works as expected", {
  now <- as.POSIXct("2019-12-11 00:12:13")

  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), "0 days", now))
  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), "-1 days", now))
  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), 0, now))
  expect_true(is_backup_older_than_interval(as.Date("2019-12-11"), -1, now))


  now <- "2019-12-11--00-12"

  expect_false(is_backup_older_than_interval(as.Date("2019-01-01"), "1 year", now))
  expect_true(is_backup_older_than_interval(as.Date("2018-12-12"), "1 year", now))
  expect_true(is_backup_older_than_interval(as.Date("2018-12-12"), "0 year", now))

  expect_false(is_backup_older_than_interval(as.Date("2999-12-12"), Inf, now))
})





test_that("is_backup_older_than_interval works with weeks", {
  # week
  expect_false(
    is_backup_older_than_interval(interval = "1 week", as.Date("2019-04-01"), as.Date("2019-04-07"))  # 2019-W14
  )
  expect_true(
    is_backup_older_than_interval(interval = "1 week", as.Date("2019-04-01"), as.Date("2019-04-08"))  # 2019-W14
  )
  expect_false(
    is_backup_older_than_interval(interval = "6 week", as.Date("2019-04-01"),  as.Date("2019-05-06")) # 2019-W19
  )
  expect_true(
    is_backup_older_than_interval(interval = "5 weeks", as.Date("2019-04-01"),  as.Date("2019-05-06")) # 2019-W19
  )

  # month
  expect_false(
    is_backup_older_than_interval(interval = "1 month", as.Date("2019-04-01"), as.Date("2019-04-30"))  # 2019-W14
  )
  expect_true(
    is_backup_older_than_interval(interval = "1 month", as.Date("2019-04-01"), as.Date("2019-05-01"))  # 2019-W14
  )
  expect_false(
    is_backup_older_than_interval(interval = "6 month", as.Date("2019-04-01"),  as.Date("2019-09-01")) # 2019-W19
  )
  expect_true(
    is_backup_older_than_interval(interval = "5 months", as.Date("2019-04-01"),  as.Date("2019-09-06")) # 2019-W19
  )
})



test_that("is_backup_older_than_datetime works as expected", {
  now <- as.POSIXct("2019-12-11 00:12:13")

  expect_true(is_backup_older_than_datetime(as.Date("2019-12-11"), now))
  expect_false(is_backup_older_than_datetime(as.Date("2019-12-11"), as.Date(now)))
  expect_false(is_backup_older_than_datetime(as.Date("2019-12-12"), now))
})

