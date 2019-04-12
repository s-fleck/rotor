test_that("parse_date works as expected", {
  expect_identical(parse_date("2018-12-01"), as.Date("2018-12-01"))
  expect_identical(parse_date("20181201"), as.Date("2018-12-01"))
  expect_identical(parse_date("2018-02"), as.Date("2018-02-01"))
  expect_identical(parse_date("201802"), as.Date("2018-02-01"))
  expect_identical(parse_date("2018"), as.Date("2018-01-01"))

  expect_identical(
    parse_date(c("2018-12-02", "20181201", "2018")),
    as.Date(c("2018-12-02", "2018-12-01", "2018-01-01"))
  )
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
