context("AppenderFileRotating")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("AppenderFileRotating works as expected", {
  tf <- file.path(td, "test.log")
  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(AppenderFileRotatingDate$new(file = tf))

  lg$fatal("test")

  # first rotate roates a file with content
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-01"))
  expect_gt(lg$appenders[[1]]$backups[1, ]$size, 0)

  # second rotate only has a file of size 0 to rotate
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-02"))
  expect_equal(lg$appenders[[1]]$backups[1, ]$size, 0)

  # compression is possible
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-03"))
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))

  lg$appenders[[1]]$prune(0)
  lg$appenders[[1]]$backups
})
