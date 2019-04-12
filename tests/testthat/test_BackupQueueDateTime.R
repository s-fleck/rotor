context("BackupQueueDateTime")


test_that("BackupQueueDateTime works as expected", {

  d1 <- as.POSIXct("2019-04-12 17:49:19")
  d2 <- as.POSIXct("2019-04-12 17:49:00")
  d3 <- as.POSIXct("2019-04-12 17:00:00")

  expect_identical(parse_datetime(d1), d1)

  expect_identical(parse_datetime("2019-04-12T17-49-19"), d1)
  expect_identical(parse_datetime("2019-04-12T17-49"), d2)
  expect_identical(parse_datetime("2019-04-12T17"), d3)

  expect_identical(parse_datetime("2019-04-12T174919"), d1)
  expect_identical(parse_datetime("2019-04-12T1749"), d2)
  expect_identical(parse_datetime("2019-04-12T17"), d3)

  expect_identical(parse_datetime("20190412T174919"), d1)
  expect_identical(parse_datetime("20190412T1749"), d2)
  expect_identical(parse_datetime("20190412T17"), d3)

  expect_identical(parse_datetime("20190412174919"), d1)
  expect_identical(parse_datetime("201904121749"), d2)
  expect_identical(parse_datetime("2019041217"), d3)

  expect_identical(
    parse_datetime(c("2019-04-12T17-49-19", "20190412T1749", "2019041217")),
    as.POSIXct(as.character(c(d1, d2, d3)))  # as.POSIXct because c removes timezone
  )
})
