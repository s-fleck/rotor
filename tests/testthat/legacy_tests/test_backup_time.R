



test_that("roate_date keeps expected number of backups", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  for (i in 1:10) {
    backup_date(
      tf,
      n_backups = 5,
      date = date + i * 5
    )
  }

  res <- find_backups(tf)

  expect_length(res, 5)

  expect_identical(
    basename(res),
    c("test.2019-01-31.log",
      "test.2019-02-05.log",
      "test.2019-02-10.log",
      "test.2019-02-15.log",
      "test.2019-02-20.log"
    )
  )

  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})



test_that("compressed roate_date works as expected", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  for (i in 1:10) {
    backup_date(
      tf,
      n_backups = 5,
      date = date + i * 5
    )
  }
  expect_length(find_backups(tf), 5)

  for (i in 1:7) {
    backup_date(
      tf,
      n_backups = 10,
      date = date + 60 + i * 5,
      compression = "zip"
    )
  }
  r <- find_backups(tf)
  expect_length(r, 10)
  expect_identical(
    tools::file_ext(find_backups(tf)),
    c(rep("log", 3), rep("zip", 7))
  )

  expect_identical(last(basename(r)), "test.2019-04-06.log.zip")
  expect_identical(first(basename(r)), "test.2019-02-10.log")


  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})




test_that("parse_date works as expected", {
  expect_identical(parse_date("2018-12-01"), as.Date("2018-12-01"))
  expect_identical(parse_date("20181201"), as.Date("2018-12-01"))
  expect_identical(parse_date("2018-02"), as.Date("2018-02-01"))
  expect_identical(parse_date("201802"), as.Date("2018-02-01"))
  expect_identical(parse_date("2018"), as.Date("2018-01-01"))
})



test_that("is_parsable_date works as expected", {
  expect_true(is_parsable_date("2018-12-01"))
  expect_true(is_parsable_date("20181201"))
  expect_true(is_parsable_date("2018-02"))
  expect_true(is_parsable_date("201802"))
  expect_true(is_parsable_date("2018"))

  expect_true(is_parsable_date(20181231))
  expect_false(is_parsable_date(20181232))
  expect_false(is_parsable_date("1 week"))
  expect_false(is_parsable_date("2 years"))
})




test_that("backup_time works", {
  skip("needs rewrite")
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  bq <- BackupQueueDate$new(tf)
  expect_identical(bq$n_backups, 0L)

  mockery::stub(bq$push_backup, "Sys.Date", date + i * 5)
  backup_time(tf, date = date  + 100, format = "%Y%m%d")
  backup_time(tf, date = date  + 110, format = "%Y-%m")
  backup_time(tf, date = date  + 120, format = "%Y%m")
  backup_time(tf, date = date  + 130, format = "%Y", n_backups = 5)
  expect_length(bq$backups$path, 5)

  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)
  on.exit(file.remove(noback))

  expect_identical(
    basename(bq$backups$path),
    c(
      "test.2019-02-20.log",
      "test.2019-04.log",
      "test.2019.log",
      "test.20190411.log",
      "test.201905.log"
    )
  )

  bq$prune(0)
  file.remove(tf)
})

