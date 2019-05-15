context("BackupQueue")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})




test_that("get_backups works as expected", {
  tf <- file.path(td, "test")
  file.create(tf)

  expect_identical(
    get_backups("foo.txt", c("foo.1.txt", "bar"), sfx_patterns = "\\d{1}"),
    "foo.1.txt"
  )

  expect_path_equal(
    get_backups("foo.txt", c("path/to/foo.1.txt", "path/to/bar"), sfx_patterns = "\\d{1}"),
    "path/to/foo.1.txt"
  )

  expect_error(
    get_backups("foo.txt", c("path/to/foo.1.txt", "path/bar"), sfx_patterns = "\\d{1}")
  )
})



test_that("BackupQueue works as expected", {
  tf <- file.path(td, "test")
  file.create(tf)

  bq <- BackupQueue$new(tf)
  expect_path_equal(bq$file, tf)
  expect_path_equal(bq$backup_dir, dirname(tf))
  file.remove(tf)
})




test_that("BackupQueue finding backups works as expected for files with extension", {
  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs, ".log")
  file.create(bus)

  expect_path_setequal(bq$backups$path, bus)
  expect_setequal(bq$backups$sfx, sfxs)
  expect_setequal(bq$backups$ext, "log")
  bq$prune(0)
})



test_that("BackupQueue finding backups works as expected for files without extension", {
  tf <- file.path(td, "test")
  file.create(tf)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs)
  file.create(bus)

  expect_path_setequal(bq$backups$path, bus)
  expect_setequal(bq$backups$sfx, sfxs)
  expect_setequal(bq$backups$ext, "")
})




test_that("dryrun/verbose prune", {
  tf <- file.path(td, "test")
  file.create(tf)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs)
  file.create(bus)

  DRY_RUN$activate()
  on.exit(DRY_RUN$deactivate())
  bq$prune(0)
  DRY_RUN$deactivate()
  expect_identical(bq$n_backups, length(sfxs))

  bq$prune(0)
  expect_identical(bq$n_backups, 0L)
})




test_that("filenames_as_matrix works as expected", {

  res <- filenames_as_matrix(
    "blah.txt",
    c(
      "blah.1.txt",
      "blah.2019-12-31.txt",
      "blah.2019-12-31--01-01-01.txt.tar.gz"
    )
  )

  expect_identical(res[, "ext"], c("txt", "txt", "txt.tar.gz"))
  expect_identical(res[, "sfx"], c("1",  "2019-12-31", "2019-12-31--01-01-01"))
})




test_that("filenames_as_matrix works as expected with paths", {
  expect_error(
    filenames_as_matrix("x/y.txt", c("a/y.1.txt", "b/y.2.txt")),
    "same directory"
  )
  expect_error(
    filenames_as_matrix("x/y.txt", c("a/y.1.txt", "a/z.2.txt")),
    "same basename"
  )

  res <- filenames_as_matrix(
    "blah/blah.txt",
    c(
      "blubb/blah.1.txt",
      "blubb/blah.2019-12-31.txt",
      "blubb/blah.2019-12-31--01-01-01.txt.tar.gz"
    )
  )

  expect_path_equal(res[, "dir"], rep("blubb", 3))
  expect_identical(res[, "name"], rep("blah", 3))
  expect_identical(res[, "ext"], c("txt", "txt", "txt.tar.gz"))
  expect_identical(res[, "sfx"], c("1",  "2019-12-31", "2019-12-31--01-01-01"))
})
