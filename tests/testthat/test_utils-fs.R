context("utils-fs")


test_that("utils-fs works as expected", {

  expect_message(
    msg_file_create("foo", "bar", dry_run = FALSE, verbose = TRUE),
    "(\\+.*){2}"
  )

  expect_message(
    msg_file_remove("foo", "bar", dry_run = FALSE, verbose = TRUE),
    "(\\-.*){2}"
  )

  expect_message(
    msg_file_rename(c("foo", "bar"), c("f00", "fizz/bar"), dry_run = FALSE, verbose = TRUE),
    "(\\~.*){2}"
  )

  expect_message(
    msg_file_copy(c("fizz/foo", "bar"), c("fizz/f00", "fizz/bar"), dry_run = FALSE, verbose = TRUE),
    "(\\+.*){2}"
  )
})
