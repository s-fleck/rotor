context("prune")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
owd <- getwd()

teardown({
  unlink(td, recursive = TRUE)
  setwd(owd)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("prune_head works as expected", {
  setwd(td)

  file.create("foo.log")
  backup("foo.log")
  backup("foo.log")
  backup("foo.log")

  r <- prune_tail("foo.log", 2)

  expect_identical(r,  c("foo.1.log", "foo.2.log"))
  file.remove(r)
  file.remove("foo.log")
  expect_length(list.files(td), 0)
})




test_that("prune_bottom works as expected", {
  setwd(td)

  file.create("foo.log")
  backup_date("foo.log", time = as.Date("2019-01-01"))
  backup_date("foo.log", time = as.Date("2019-01-02"))
  backup_date("foo.log", time = as.Date("2019-01-03"))

  find_backups("foo.log")

  r <- prune_head("foo.log", 2)

  expect_identical(r,  c("foo.2019-01-02.log", "foo.2019-01-03.log"))
  file.remove(r)
  file.remove("foo.log")
  expect_length(list.files(td), 0)
})
