context("utils-fs")


dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




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



test_that("utils-fs can create/remove files in dry_run memory", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  snap <- fileSnapshot(td)
  DRY_RUN$activate()

  on.exit({
    file.remove(tf)
    DRY_RUN$deactivate()
  })


  # create fake file
  expect_true(file_create(file.path(td, "test2.log")))
  expect_true(file_exists(file.path(td, "test2.log")))
  expect_snapshot_unchanged(snap)

  # delete real file only in memory
  expect_true(file_remove(file.path(td,  "test.log")))
  expect_false(file_exists(file.path(td, "test.log")))
  expect_true(file.exists(file.path(td, "test.log")))
  expect_snapshot_unchanged(snap)

  # delete fake file from memory
  expect_true(file_remove(file.path(td,  "test2.log")))
  expect_false(file_exists(file.path(td, "test2.log")))
  expect_snapshot_unchanged(snap)

  # list files
  expect_identical(list_files(td), character())

  new_real <- file.path(td, "foo.txt")
  new_fake <- file.path(td, "bar.txt")
  on.exit(file.remove(new_real), add = TRUE)

  file.create(new_real)
  file_create(new_fake)
  expect_snapshot_unchanged(snap)

  expect_path_equal(list_files(td, full.names = TRUE), c(new_real, new_fake))
  expect_path_equal(list.files(td, full.names = TRUE), c(new_real, tf))
})
