context("utils-predicates")


test_that("utils-predicates works as expected", {

  ori_zipcmd <- Sys.getenv("R_ZIPCMD")
  on.exit(Sys.setenv(R_ZIPCMD = ori_zipcmd))

  expect_true(is_zipcmd_available())
  Sys.setenv(R_ZIPCMD = "aslkjghasdkjlghas")
  expect_false(is_zipcmd_available())




})
