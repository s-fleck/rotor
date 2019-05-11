context("DryRunMemory")


dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)
teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("DryRunMemory works as expected", {
  file.create(file.path(td, "test.log"))
  snap <- fileSnapshot(td)

  dm <- DryRunMemory$new()

  dm$create("test.log2")
  dm$create("test.log3")

  dm$list_files(td)

  dm$move(file.path(td, "test.log"), file.path(td, "test99.log"))

  expect_identical(sum(grepl("test99", dm$list_files(td))), 1L)
  expect_identical(sum(grepl("test.log2", dm$list_files(td))), 1L)
  expect_identical(sum(grepl("test.log3", dm$list_files(td))), 1L)
  expect_length(dm$list_files(td), 3L)

  dm$delete(file.path(td, "test99.log"))
  expect_length(dm$list_files(td), 2L)

  expect_snapshot_unchanged(snap)
})
