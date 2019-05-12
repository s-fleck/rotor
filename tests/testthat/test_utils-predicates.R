context("utils-predicates")




test_that("is_zipcmd_available detects zipcommand", {
  # can only return true on platforms with a zip command
  skip_if_not(is_zipcmd_available(), "No zipcommand found")
  expect_true(is_zipcmd_available())
})




test_that("is_zipcmd_available() detects missing zipcommand", {
  expect_false(is_zipcmd_available("sdjkghsaghaskjghsagj"))
})




test_that("utils-fs can create/remove files in dry_run memory", {
  td <- file.path(tempdir(), "rotor")
  tf1 <- file.path(td, "foo")
  tf2 <- file.path(td, "bar")
  dir.create(tf1, recursive = TRUE)
  file.create(tf2)

  on.exit(unlink(c(tf2, tf1, td), recursive = TRUE))

  expect_true(is_dir(tf1))
  expect_false(is_dir(tf2))
})
