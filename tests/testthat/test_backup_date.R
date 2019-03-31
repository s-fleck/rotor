context("rotate_date")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("roate_date keeps expected number of backups", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  for (i in 1:10) {
    backup_date(
      tf,
      max_backups = 5,
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
      max_backups = 5,
      date = date + i * 5
    )
  }
  expect_length(find_backups(tf), 5)

  for (i in 1:7) {
    backup_date(
      tf,
      max_backups = 10,
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
