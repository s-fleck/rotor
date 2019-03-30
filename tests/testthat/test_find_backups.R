context("find_backups")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
owd <- getwd()

teardown({
  unlink(td, recursive = TRUE)
  setwd(owd)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("get_backups works as expected", {
  src <- "blah.log"
  cld <- c("blah.1.log.zip", "blah.2.log.zip", "blah.3.log.zip", "blah.3.log.tar.gz", "blubb.3.log.zip")

  expect_identical(
    get_backups(src, cld),
    sort(cld[1:4])
  )
})




test_that("get_backups works as expected", {
  file.create("foo.log")
  backup("foo.log")
  backup("foo.log")
  backup("foo.log")

  r <- find_backups("foo.log")

  expect_identical(r,  c("foo.1.log", "foo.2.log", "foo.3.log"))
  file.remove(r)
  file.remove("foo.log")
  expect_length(list.files(td), 0)
})
