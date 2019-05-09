context("utils-predicates")


test_that("is_zipcmd_available detects zipcommand", {
  # can only return true on platforms with a zip command
  skip_if_not(is_zipcmd_available(), "No zipcommand found")
  expect_true(is_zipcmd_available())
})




test_that("is_zipcmd_available() detects missing zipcommand", {
  expect_false(is_zipcmd_available("sdjkghsaghaskjghsagj"))
})
