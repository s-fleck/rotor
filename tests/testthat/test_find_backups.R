context("find_backups")


test_that("get_backups works as expected", {
  src <- "blah.log"
  cld <- c("blah.1.log.zip", "blah.2.log.zip", "blah.3.log.zip", "blah.3.log.tar.gz", "blubb.3.log.zip")

  expect_identical(
    get_backups(src, cld),
    sort(cld[1:4])
  )
})
