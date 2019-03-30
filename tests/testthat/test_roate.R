context("roate")


test_that("roate works as expected", {

  td <- file.path(tempdir(), "rotor")
  tf <- file.path(td, "test.log")
  dir.create(td, recursive = TRUE)
  file.create(tf)

  for (i in 1:10){
    backup(tf, verbose = TRUE, max_backups = 5)
  }

  expect_length(find_children(tf), 5)

  for (i in 1:50){
    backup(tf, verbose = TRUE, max_backups = 12)
  }

  expect_length(find_children(tf), 12)

  prune_backups(tf, 5)
  find_children(tf)

  unlink(td)
})
