context("name_components")


test_that("name_components works as expected", {
  fnames <- c(
    "blah",
    "blah.log",
    "blah.100",
    "blah.100.log",
    "blah.100.zip",
    "path/to/blah.100.zip",
    "path/to/blah.100.tar.gz",
    "zip",
    "zip.100",
    "balltar.gz"
  )
})




test_that("get_descendents works as expected", {
  x <- c("blah.1.log.zip", "blah.2.log.zip", "blah.3.log.zip", "blah.3.log.tar.gz", "blubb.3.log.zip")
  src <- "blah.log"

  expect_identical(
    get_descendents(x, src),
    x[1:4]
  )
})




test_that("name_components works as expected", {
  x <- c("blah.1.log.zip", "blah.2.log.zip", "blah.3.log.zip", "blah.4.log.tar.gz")
  src <- "blah.log"

  r <- get_name_components(x, "blah.log")

  expect_true(all_are_identical(r[, "name"]))
  expect_identical(r[, "sfx"], as.character(1:4))

  r
})
