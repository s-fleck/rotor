context("rotate_size")




test_that("rotate_size works as expected", {
  tf <- tempfile()
  on.exit(remove(tf))

  saveRDS(iris, tf)
  size <- file.size(tf)

  expect_identical(
    rotate_size(tf, size = round(1.5 * size), verbose = TRUE),
    character()
  )
  expect_identical(
    rotate_size(tf, size = round(0.5 * size), verbose = TRUE),
    paste0(tf, ".1")
  )
  expect_length(find_backups(tf), 1)

  # rotate and zip
  saveRDS(iris, tf)

  expect_identical(
    rotate_size(tf, size = round(0.5 * size), verbose = TRUE, compress = TRUE),
    paste0(tf, ".1.zip")
  )
  expect_equal(file.size(tf), 0)
  find_backups(tf)


  file.remove(find_backups(tf))
  file.remove(tf)
})
