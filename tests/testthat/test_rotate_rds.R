context("rotate_rds")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
tf <- file.path(td, "iris.rds")

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("rotate_rds works as expected", {
  on.exit(unlink(c(tf, list_backups(tf))))

  rotate_rds(iris, tf)
  rotate_rds(iris, tf)
  rotate_rds(iris, tf)

  expect_identical(n_backups(tf), 2L)
})



test_that("rotate_rds_date works as expected", {
  now <- Sys.Date()
  on.exit(unlink(c(tf, list_backups(tf))))

  rotate_rds_date(iris, tf, now = now)
  rotate_rds_date(iris, tf, now = now)
  expect_identical(n_backups(tf), 1L)
  expect_error(rotate_rds_date(iris, tf, now = now))
  rotate_rds_date(iris, tf, now = now + 1L)

  expect_identical(n_backups(tf), 2L)
})



test_that("rotate_rds_time works as expected", {
  now <- Sys.time()
  on.exit(unlink(c(tf, list_backups(tf))))

  rotate_rds_time(iris, tf, now = now)
  rotate_rds_time(iris, tf, now = now)
  expect_error(rotate_rds_time(iris, tf, now = now))
  rotate_rds_time(iris, tf, now = now + 1L)

  expect_identical(n_backups(tf), 2L)
})




test_that("rotate_rds on_change_only", {
  v <- LETTERS
  df <- iris
  dt <- data.table::as.data.table(iris)

  td <- file.path(tempdir(), "rotate_rds_test_temp")
  dir.create(td)
  tf <- file.path(td, "testfile.rds")

  on.exit(unlink(td, recursive = TRUE))

  expect_identical(expect_silent(rotate_rds(v, tf, on_change_only = TRUE)), tf)
  expect_identical(expect_message(rotate_rds(v, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage"), tf)

  expect_silent(rotate_rds(df, tf, on_change_only = TRUE))
  expect_message(rotate_rds(df, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)

  expect_silent(rotate_rds(dt, tf, on_change_only = TRUE))
  expect_message(rotate_rds(dt, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)
})




test_that("rotate_rds on_change_only", {
  v <- LETTERS
  df <- iris
  dt <- data.table::as.data.table(iris)

  td <- file.path(tempdir(), "rotate_rds_test_temp")
  dir.create(td)
  tf <- file.path(td, "testfile.rds")

  on.exit(unlink(td, recursive = TRUE))

  expect_identical(expect_silent(rotate_rds_time(v, tf, on_change_only = TRUE)), tf)
  expect_identical(expect_message(rotate_rds_time(v, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage"), tf)

  expect_silent(rotate_rds_time(df, tf, on_change_only = TRUE))
  expect_message(rotate_rds_time(df, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)

  expect_silent(rotate_rds_time(dt, tf, on_change_only = TRUE))
  expect_message(rotate_rds_time(dt, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)
})




test_that("rotate_rds on_change_only", {
  v <- LETTERS
  df <- iris
  dt <- data.table::as.data.table(iris)

  td <- file.path(tempdir(), "rotate_rds_test_temp")
  dir.create(td)
  tf <- file.path(td, "testfile.rds")

  on.exit(unlink(td, recursive = TRUE))

  expect_identical(expect_silent(rotate_rds_date(v, tf, on_change_only = TRUE)), tf)
  expect_identical(expect_message(rotate_rds_date(v, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage"), tf)

  expect_silent(rotate_rds_date(df, tf, on_change_only = TRUE))
  expect_message(rotate_rds_date(df, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)

  expect_silent(rotate_rds_date(dt, tf, on_change_only = TRUE))
  expect_message(rotate_rds_date(dt, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)
})
