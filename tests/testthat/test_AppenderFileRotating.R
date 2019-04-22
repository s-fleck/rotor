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

  AppenderFileRotatingDate$new(file = tf)

  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(AppenderFileRotatingDate$new(file = tf))

  lg$fatal("test")

  lg$appenders[[1]]$rotate()
  lg$appenders[[1]]$backups

  lg$appenders[[1]]$prune(0)
  lg$appenders[[1]]$backups

})
