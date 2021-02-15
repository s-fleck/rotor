context("rotate_rds")

td <- file.path(tempdir(), "rotor")
tf <- file.path(td, "iris.rds")

teardown({
  unlink(td, recursive = TRUE)
})



test_that("rotate_rds works as expected", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  rotate_rds(iris, tf)
  rotate_rds(iris, tf)
  rotate_rds(iris, tf)

  expect_identical(n_backups(tf), 2L)
})



test_that("rotate_rds_date works as expected", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  now <- Sys.Date()

  rotate_rds_date(iris, tf, now = now)
  rotate_rds_date(iris, tf, now = now)
  expect_identical(n_backups(tf), 1L)
  expect_error(rotate_rds_date(iris, tf, now = now))
  rotate_rds_date(iris, tf, now = now + 1L)

  expect_identical(n_backups(tf), 2L)
})



test_that("rotate_rds_time works as expected", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  now <- Sys.time()

  rotate_rds_time(iris, tf, now = now)
  rotate_rds_time(iris, tf, now = now)
  expect_error(rotate_rds_time(iris, tf, now = now))
  rotate_rds_time(iris, tf, now = now + 1L)

  expect_identical(n_backups(tf), 2L)
})




test_that("rotate_rds on_change_only", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  v <- LETTERS
  df <- iris
  dt <- data.table::as.data.table(iris)
  tf <- file.path(td, "testfile.rds")

  expect_identical(expect_silent(rotate_rds(v, tf, on_change_only = TRUE)), tf)
  expect_identical(expect_message(rotate_rds(v, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage"), tf)

  expect_silent(rotate_rds(df, tf, on_change_only = TRUE))
  expect_message(rotate_rds(df, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)

  expect_silent(rotate_rds(dt, tf, on_change_only = TRUE))
  expect_message(rotate_rds(dt, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)
})




test_that("rotate_rds_time on_change_only", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  v <- LETTERS
  df <- iris
  dt <- data.table::as.data.table(iris)
  tf <- file.path(td, "testfile.rds")

  expect_identical(expect_silent(rotate_rds_time(v, tf, on_change_only = TRUE)), tf)
  expect_identical(expect_message(rotate_rds_time(v, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage"), tf)

  expect_silent(rotate_rds_time(df, tf, on_change_only = TRUE))
  expect_message(rotate_rds_time(df, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)

  expect_silent(rotate_rds_time(dt, tf, on_change_only = TRUE))
  expect_message(rotate_rds_time(dt, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)
})




test_that("rotate_rds_date on_change_only", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  v <- LETTERS
  df <- iris
  dt <- data.table::as.data.table(iris)
  tf <- file.path(td, "testfile.rds")

  expect_identical(expect_silent(rotate_rds_date(v, tf, on_change_only = TRUE)), tf)
  expect_identical(expect_message(rotate_rds_date(v, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage"), tf)

  expect_silent(rotate_rds_date(df, tf, on_change_only = TRUE))
  expect_message(rotate_rds_date(df, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)

  expect_silent(rotate_rds_date(dt, tf, on_change_only = TRUE))
  expect_message(rotate_rds_date(dt, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  prune_backups(tf, 0)
})



test_that("rotate_rds `on_change_only` works with arguments list", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  dt1 <- data.table::as.data.table(iris)
  dt2 <- dt1[rev(seq_len(nrow(dt1))), ]
  tf <- file.path(td, "testfile.rds")

  expect_silent(rotate_rds(dt1, tf, on_change_only = TRUE))
  expect_message(rotate_rds(dt1, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  expect_message(rotate_rds(dt2, tf, on_change_only = list(ignore.row.order = TRUE)), class = "ObjectHasNotChangedMessage")
  expect_message(rotate_rds(dt1, tf, on_change_only = TRUE), class = "ObjectHasNotChangedMessage")
  expect_silent(rotate_rds(dt2, tf, on_change_only = TRUE))

  expect_identical(n_backups(tf), 1L)
  prune_backups(tf, 0)
})




test_that("objects_are_equal ", {
  x <- data.table::data.table(a = 1:3)
  y <- data.table::data.table(a = 3:1)

  expect_false(objects_are_equal(x, y, extra_args = list()))
  expect_true(objects_are_equal(x, y, extra_args = list(ignore.row.order = TRUE)))
})
