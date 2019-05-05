context("newest_backup")

dr <- tempdir()
td <- file.path(dr, "rotor")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



test_that("n_backups and co work as expected", {
  tf <- file.path(td, "test.log")
  files <- file.path(
    td,
    c("test.2019-02-01--12-00-00.log", "test.2019-02-01--12-00-01.log")
  )

  file.create(tf, files)

  expect_path_equal(newest_backup(tf), files[[2]])
  expect_path_equal(oldest_backup(tf), files[[1]])

  files2 <- file.path(td, c("test.1.log", "test.2.log"))
  file.create(files2)

  expect_error(newest_backup(tf))
  expect_error(prune_backups(tf, 0))
  expect_warning(expect_true(n_backups(tf) == 4))
  file.remove(files)

  expect_path_equal(newest_backup(tf), files2[[1]])
  expect_path_equal(oldest_backup(tf), files2[[2]])
  prune_backups(tf, 0)
  expect_true(n_backups(tf) == 0)

  file.remove(tf)
})




test_that("n_backups and co work as expected with backup_dor", {
  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  on.exit(unlink(c(bu_dir, tf)))

  files <- file.path(
    bu_dir,
    c("test.2019-02-01--12-00-00.log", "test.2019-02-01--12-00-01.log")
  )

  file.create(tf, files)

  expect_path_equal(newest_backup(tf, backup_dir = bu_dir), files[[2]])
  expect_path_equal(oldest_backup(tf, backup_dir = bu_dir), files[[1]])

  files2 <- file.path(bu_dir, c("test.1.log", "test.2.log"))
  file.create(files2)

  expect_error(newest_backup(tf, backup_dir = bu_dir))
  expect_error(prune_backups(tf, 0, backup_dir = bu_dir))
  expect_warning(expect_true(n_backups(tf, backup_dir = bu_dir) == 4))
  file.remove(files)

  expect_path_equal(newest_backup(tf, backup_dir = bu_dir), files2[[1]])
  expect_path_equal(oldest_backup(tf, backup_dir = bu_dir), files2[[2]])
  prune_backups(tf, 0, backup_dir = bu_dir)
  expect_true(n_backups(tf) == 0)

  file.remove(tf)
})
