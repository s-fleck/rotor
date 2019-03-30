context("name_components")




test_that("get_descendents works as expected", {
  src <- "blah.log"
  cld <- c("blah.1.log.zip", "blah.2.log.zip", "blah.3.log.zip", "blah.3.log.tar.gz", "blubb.3.log.zip")

  expect_identical(
    get_children(src, cld),
    sort(cld[1:4])
  )
})




test_that("name_components works as expected", {
  src <- "blah.log"
  cld <- c("blah.1.log.zip", "blah.2.log.zip", "blah.3.log.zip", "blah.4.log.tar.gz")

  r <- get_name_components(src, cld)

  expect_true(all_are_identical(r[, "name"]))
  expect_identical(r[, "sfx"], as.character(1:4))

  r
})
