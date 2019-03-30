context("name_components")



test_that("name_components works as expected", {
  src <- "blah.log"
  cld <- c("blah.1.log.zip", "blah.2.log.zip", "blah.3.log.zip", "blah.4.log.tar.gz")

  r <- get_name_components(src, cld)

  expect_true(all_are_identical(r[, "name"]))
  expect_identical(r[, "sfx"], as.character(1:4))
})
