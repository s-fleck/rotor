context("rotate_date")

td <- file.path(tempdir(), "rotor")
dir.create(td, recursive = TRUE)
teardown(unlink(td, recursive = TRUE))




test_that("roate_date keeps expected number of backups", {
  tf <- file.path(td, "test.log")
  file.create(tf)

  time <- as.POSIXct(as.Date("2019-01-01"))

  for (i in 1:10) {
    backup_date(
      tf,
      max_backups = 5,
      format = "%Y%m%dT%H%M%S",
      time = time + i * 5
    )
  }

  res <- find_backups(tf)

  expect_length(res, 5)

  expect_identical(
    basename(res),
    c("test.20190101T010030.log",
      "test.20190101T010035.log",
      "test.20190101T010040.log",
      "test.20190101T010045.log",
      "test.20190101T010050.log"
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
  time <- as.POSIXct(as.Date("2019-01-01"))

  for (i in 1:10) {
    backup_date(
      tf,
      max_backups = 5,
      format = "%Y%m%dT%H%M%S",
      time = time + i * 5
    )
  }
  expect_length(find_backups(tf), 5)

  for (i in 1:7) {
    backup_date(
      tf,
      max_backups = 10,
      format = "%Y%m%dT%H%M%S",
      time = time + 60 + i * 5,
      compression = "zip"
    )
  }
  r <- find_backups(tf)
  expect_length(r, 10)

  expect_identical(
    tools::file_ext(find_backups(tf)),
    c(rep("log", 3), rep("zip", 7))
  )

  expect_identical(last(basename(r)), "test.20190101T010135.log.zip")
  expect_identical(first(basename(r)), "test.20190101T010040.log")


  # cleanup
  file.remove(find_backups(tf))
  file.remove(tf)
  expect_length(list.files(td), 0)
})
