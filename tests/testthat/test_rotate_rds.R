context("rotate_rds")

dr <- tempdir()
td <- file.path(dr, "rotor")
tf <- "iris.rds"
dir.create(td, recursive = TRUE)

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
