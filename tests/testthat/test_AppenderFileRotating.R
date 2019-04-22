context("AppenderFileRotating")


test_that("AppenderFileRotating works as expected", {

  tf <- tempfile()

  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(AppenderFileRotatingDate$new(file = tf))

  lg$fatal("test")

  lg$appenders[[1]]$rotate()



})
