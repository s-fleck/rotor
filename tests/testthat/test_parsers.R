context("parsers")


test_that("parse_interval", {
  expect_identical(parse_interval(9)$unit, "day")
  expect_identical(parse_interval("1 week")$unit, "week")
  expect_identical(parse_interval("2 months")$unit, "month")
  expect_identical(parse_interval("3 quarters")$unit, "quarter")
  expect_identical(parse_interval("4 years")$unit, "year")
})




test_that("parse_info_unit works", {
  expect_identical(parse_info_unit("k"), 1024)
  expect_identical(parse_info_unit("k"), parse_info_unit("KiB"))
  expect_identical(parse_info_unit("m"), 1024 * 1024)
  expect_identical(parse_size(123), 123L)
  expect_error(parse_info_unit("r"))
  expect_identical(parse_size("1k"), 1024)
  expect_equal(parse_size("1.5g"), 1024L^3 * 1.5)

  expect_equal(parse_size("1.5 gIb"), parse_size("1.5g"))
  expect_equal(parse_size("1 gIb"), parse_size("1024mb"))
})





test_that("parse_datetime works as expected", {
  d <- as.Date("2019-12-01")
  expect_equal(parse_datetime(d), as.POSIXct(as.character(d)))

  expect_equal(parse_datetime("2018-12-01"), as.POSIXct("2018-12-01"))
  expect_equal(parse_datetime("20181201"), as.POSIXct("2018-12-01"))
  expect_equal(parse_datetime("2018-02"), as.POSIXct("2018-02-01"))
  expect_equal(parse_datetime("201802"), as.POSIXct("2018-02-01"))
  expect_equal(parse_datetime("2018"), as.POSIXct("2018-01-01"))

  expect_equal(
    parse_datetime(c("2018-12-02", "20181201", "2018")),
    as.POSIXct(c("2018-12-02", "2018-12-01", "2018-01-01"))
  )

  d1 <- as.POSIXct("2019-04-12 17:49:19")
  d2 <- as.POSIXct("2019-04-12 17:49:00")
  d3 <- as.POSIXct("2019-04-12 17:00:00")

  expect_identical(parse_datetime(d1), d1)

  expect_equal(parse_datetime("2019-04-12--17-49-19"), d1)
  expect_equal(parse_datetime("2019-04-12--17-49"), d2)
  expect_equal(parse_datetime("2019-04-12----17"), d3)

  expect_equal(parse_datetime("2019-04-12T17-49-19"), d1)
  expect_equal(parse_datetime("2019-04-12T17-49"), d2)
  expect_equal(parse_datetime("2019-04-12T17"), d3)

  expect_equal(parse_datetime("2019-04-12T174919"), d1)
  expect_equal(parse_datetime("2019-04-12T1749"), d2)
  expect_equal(parse_datetime("2019-04-12T17"), d3)

  expect_equal(parse_datetime("20190412T174919"), d1)
  expect_equal(parse_datetime("20190412T1749"), d2)
  expect_equal(parse_datetime("20190412T17"), d3)

  expect_equal(parse_datetime("20190412174919"), d1)
  expect_equal(parse_datetime("201904121749"), d2)
  expect_equal(parse_datetime("2019041217"), d3)

  expect_equal(
    parse_datetime(c("2019-04-12T17-49-19", "20190412T1749", "2019041217")),
    as.POSIXct(c(d1, d2, d3))
  )
})
