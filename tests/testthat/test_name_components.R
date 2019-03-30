context("name_components")


test_that("name_components works as expected", {
  fnames <- c(
    "blah",
    "blah.log",
    "blah.100",
    "blah.log.100",
    "blah.log.100.zip",
    "path/to/blah.log.100.zip",
    "path/to/blah.log.100.tar.gz",
    "zip",
    "zip.100"
  )

  r <- get_arc_ext(fnames)

  expect_true(r[[5]] == "zip")
  expect_true(r[[6]] == "zip")
  expect_true(r[[7]] == "tar.gz")
  expect_true(r[[8]] == "")
  expect_true(r[[9]] == "")
})







test_that("is_dater_sfx", {
  a <- "2019-02-31"
  b <- "2019-02"
  c <- "201902"
  d <- "2019W02"
})

