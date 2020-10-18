context("BackupQueue")

td <- file.path(tempdir(), "rotor")

teardown({
  unlink(td, recursive = TRUE)
})



# BackupQueue -------------------------------------------------------------

test_that("get_backups works as expected", {
  expect_identical(
    get_backups("foo.txt", c("foo.1.txt", "bar"), sfx_patterns = "\\d{1}"),
    "foo.1.txt"
  )

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

  expect_setequal(
    get_backups(
      "foo.txt",
      c("foo.1.txt", "foo.1.txt.zip", "foo.1.txt.tar.gz", "foo.1.txt.7z", "foo.7z"),
      sfx_patterns = "\\d+"
    ),
    c("foo.1.txt", "foo.1.txt.zip", "foo.1.txt.tar.gz", "foo.1.txt.7z")
  )

  expect_setequal(
    get_backups(
      "foo",
      c("foo.1", "foo.1.zip", "foo.1.tar.gz", "foo.1.7z", "foo.7z"),
      sfx_patterns = "\\d+"
    ),
    c("foo.1", "foo.1.zip", "foo.1.tar.gz", "foo.1.7z")
  )

  sfx_real <- c(
    "\\d+",
    "\\d{4}-\\d{2}-\\d{2}",
    "\\d{4}-\\d{2}-\\d{2}(-*|",
    "\\d{4}-\\d{2}"
  )

  expect_length(
    get_backups("test.jsonl", "test.7z", sfx_patterns = sfx_real), 0
  )

  bus <- c(
    "test.1.log.zip", "test.2.log.tar.gz", "test.2019-01-01T22-22-22.log.zip",
    "test.2019-02-01--00-00-00.log", "test.2019-03-01--00-00-00.log.zip",
    "test.3.log"
  )

  bad_bus <- c(
    "test.log.zip", "test.7z", "test.2019-01-01X22-22-22.log.zip", "test.2019-01-01T22-22-222.log.zip"
  )

  bus     <- file.path("fake","path", bus)
  bad_bus <- file.path("fake","path", bad_bus)

  expect_identical(
    get_backups("foo/bar/test.log", c(bus, bad_bus), sfx_real),
    sort(bus)
  )
})





test_that("BackupQueue works as expected", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  expect_identical(n_backups(tf), 0L)

  bq <- BackupQueue$new(tf)
  expect_path_equal(bq$origin, tf)
  expect_path_equal(bq$dir, dirname(tf))
})




test_that("BackupQueue finding backups works as expected for files with extension", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)

  expect_identical(n_backups(tf), 0L)
  bq <- BackupQueue$new(tf)
  on.exit(bq$prune(0), add = TRUE)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs, ".log")
  file.create(bus)

  expect_path_setequal(bq$files$path, bus)
  expect_setequal(bq$files$sfx, sfxs)
  expect_setequal(bq$files$ext, "log")
})




test_that("BackupQueue finding backups works as expected for files without extension", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)

  expect_identical(n_backups(tf), 0L)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs)
  file.create(bus)

  expect_path_setequal(bq$files$path, bus)
  expect_setequal(bq$files$sfx, sfxs)
  expect_setequal(bq$files$ext, "")
})




test_that("dryrun/verbose prune", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  expect_identical(n_backups(tf), 0L)
  bq <- BackupQueue$new(tf)

  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs)
  file.create(bus)

  DRY_RUN$activate()
  on.exit(DRY_RUN$deactivate(), add = TRUE)
  bq$prune(0)
  DRY_RUN$deactivate()
  expect_identical(bq$n, length(sfxs))

  bq$prune(0)
  expect_identical(bq$n, 0L)
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




test_that("$print() does not fail", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)

  bq <- BackupQueue$new(tf)

  # printing empty bq succeedes
  expect_output(print(bq))

  # printing nonempty bq succeedes
  sfxs <-c(1:12, "2019-12-31")
  bus <- paste0(tools::file_path_sans_ext(tf), ".", sfxs)
  file.create(bus)
  expect_output(print(bq))
})




# BackupQueueIndex --------------------------------------------------------
context("BackupQueueIndex")

test_that("BackupQueueIndex can find and prune backup trails", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$n, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log.zip", ".2.log.tar.gz", ".3.log"))
  file.create(bus)
  expect_path_equal(bt$files$path, bus)

  # multiple pruning with the same settings does not change anything
  expect_path_equal(bt$prune(2)$files$path, bus[1:2])
  expect_path_equal(bt$prune(2)$files$path, bus[1:2])
  expect_path_equal(bt$prune(2)$files$path, bus[1:2])

  # pruning with higher prune number than number of backups does not change anything
  expect_path_equal(bt$prune(1)$files$path, bus[1])
  expect_path_equal(bt$prune(2)$files$path, bus[1])

  #cleanup
  expect_length(bt$prune(0)$files$path, 0)
  expect_length(bt$prune(0)$files$path, 0)
})




test_that("BackupQueueIndex only shows indexed backups", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")

  file.create(c(
    tf,
    file.path(td, "test.1.log"),
    file.path(td, "test.2.log"),
    file.path(td, "test.2017.log"),
    file.path(td, "test.201701.log"),
    file.path(td, "test.20170201.log"),
    file.path(td, "test.2017-03.log"),
    file.path(td, "test.2017-04-01.log")
  ))

  bq <- BackupQueueIndex$new(tf)
  expect_true(all(bq$files$sfx == as.integer(bq$files$sfx)))

  BackupQueue$new(tf)$prune(0)
  unlink(tf)
})



test_that("BackupQueue pruning works as expected for files without extension", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$n, 0L)
  bus <- paste0(tf, c(".1", ".2", ".3"))
  file.create(bus)
  expect_path_equal(bt$files$path, bus)
  bt$prune(2)
  expect_path_equal(bt$files$path, bus[1:2])
  bt$prune(2)
  expect_path_equal(bt$files$path, bus[1:2])
  expect_length(bt$prune(0)$files$path, 0)
  file.remove(tf)
})




test_that("BackupQueue works as expected for files with extension", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  expect_identical(n_backups(tf), 0L)
  bt <- BackupQueueIndex$new(tf)

  # finding and pruning backups works
  expect_identical(bt$n, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log", ".2.log", ".3.log"))
  file.create(bus)
  expect_path_equal(bt$files$path, bus)
  bt$prune(2)
  expect_path_equal(bt$files$path, bus[1:2])
  expect_length(bt$prune(0)$files$path, 0)
  file.remove(tf)
})




test_that("BackupQueue$pad_index works as expected", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  expect_identical(n_backups(tf), 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:12, ".log")
  padded_bus <- sort(paste0(
    tools::file_path_sans_ext(tf), ".", pad_left(1:12, pad = 0), ".log"
  ))
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  expect_path_setequal(bt$files$path, bus)
  expect_path_setequal(bt$pad_index()$files$path, padded_bus)
  expect_path_setequal(bt$prune(9)$files$path, bus[1:9])

  expect_length(bt$prune(0)$files$path, 0)
  file.remove(tf)
})



test_that("BackupQueue$increment_index works as expected", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  expect_identical(n_backups(tf), 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:9, ".log")
  pushed_bus <- paste0(tools::file_path_sans_ext(tf), ".", pad_left(2:10, pad = "0"), ".log")
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  expect_path_setequal(bt$files$path, bus)
  expect_path_equal(bt$increment_index()$files$path, pushed_bus)

  expect_error(bt$increment_index(-1), "positive integers")


  expect_length(bt$prune(0)$files$path, 0)
  file.remove(tf)
})




test_that("BackupQueue$push() works as expected", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  tf <- file.path(td, "test.log")
  file.create(tf)
  expect_identical(n_backups(tf), 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), ".", 1:9, ".log")
  padded_bus <- sort(paste0(
    tools::file_path_sans_ext(tf), ".", pad_left(1:10, pad = 0), ".log"
  ))
  file.create(bus)

  bt <- BackupQueueIndex$new(tf)
  bt$push()
  expect_length(bt$files$path, 10)

  bt$set_compression(TRUE)
  bt$push()
  expect_length(bt$files$path, 11)
  expect_identical(tools::file_ext(bt$files$path[[1]]), "zip")
  expect_setequal(tools::file_ext(bt$files$path[2:11]), "log")

  expect_length(bt$prune(0)$files$path, 0)
  file.remove(tf)
})




test_that("BackupQueueIndex$push() can push to different directory", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  tf <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  file.create(tf)
  expect_identical(n_backups(tf), 0L)
  on.exit(unlink(c(bu_dir, tf), recursive = TRUE), add = TRUE)


  bt <- BackupQueueIndex$new(tf, dir = bu_dir)
  bt$push()

  expect_match(dirname(bt$files$path), "rotor.backups")
  bt$set_compression(TRUE)
  bt$push()

  expect_identical(bt$n, 2L)

  expect_length(bt$prune(0)$files$path, 0)
  expect_length(list.files(bu_dir), 0)
})




test_that("BackupQueueIndex dry run doesnt modify file system", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  expect_identical(n_backups(tf), 0L)
  bt <- BackupQueueIndex$new(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".1.log.zip", ".2.log.tar.gz", ".3.log"))
  file.create(bus)

  DRY_RUN$activate()
  on.exit({
    unlink(tf)
    unlink(bus)
    DRY_RUN$deactivate()
  }, add = TRUE)

  snap <- utils::fileSnapshot(td, md5sum = TRUE)

  expect_message(bt$increment_index(92), "93")
  expect_snapshot_unchanged(snap)

  expect_silent(bt$pad_index())
  expect_snapshot_unchanged(snap)

  expect_message(bt$push(), "test.log -> test.1.log")
  expect_snapshot_unchanged(snap)

  expect_message(bt$prune(0), "test.01.log")
  expect_snapshot_unchanged(snap)
})




test_that("BackupQueueIndex: $should_rotate", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)
  expect_identical(n_backups(tf), 0L)

  bq <- BackupQueueIndex$new(tf)
  expect_false(bq$should_rotate("1gb"))
  expect_true(bq$should_rotate("0.5kb"))
})





test_that("BackupQueueIndex: $should_rotate(verbose = TRUE) displays helpful messages", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)
  bq <- BackupQueueIndex$new(tf)

  expect_message(bq$should_rotate(size = "1 tb", verbose = TRUE), "1 TiB")
  expect_message(bq$should_rotate(size = Inf, verbose = TRUE), "infinite")
})




test_that("BackupQueueIndex: $prune_identical works", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")

  saveRDS(iris, tf)
  iris_md5 <- tools::md5sum(tf)
  bq <- BackupQueueIndex$new(tf)
  on.exit({
    bq$prune(0)
    unlink(tf)
  }, add = TRUE)
  backup(tf)
  backup(tf)
  rotate(tf)

  saveRDS(cars, tf)
  cars_md5 <- tools::md5sum(tf)
  backup(tf)
  saveRDS(cars, tf)
  rotate(tf)

  saveRDS(iris, tf)

  bq$prune_identical()

  expect_identical(
    unname(tools::md5sum(bq$files$path)),
    unname(c(cars_md5, iris_md5))
  )
})



# BackupQueueDateTime -----------------------------------------------------
context("BackupQueueDateTime")




test_that("BackupQueueDatetime: backups_cache", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)

  expect_identical(n_backups(tf), 0L)
  bq <- BackupQueueDateTime$new(tf, cache_backups = TRUE)
  file.create(paste0(tools::file_path_sans_ext(tf), ".2019-01-01T22-22-22.log.zip"))


  # last rotation cache must be updated manually
  expect_null(bq$last_rotation)
  bq$update_backups_cache()
  expect_equal(bq$last_rotation, parse_datetime("2019-01-01T22-22-22"))

  # last rotation cache is set on creation
  bq <- BackupQueueDateTime$new(tf, cache_backups = TRUE)
  expect_equal(bq$last_rotation, parse_datetime("2019-01-01T22-22-22"))

  # last rotation cache is updated on backup push
  bq <- BackupQueueDateTime$new(tf, cache_backups = TRUE)
  bq$push(now = "2019-02-01--00-00-00")
  expect_equal(bq$last_rotation, parse_datetime("2019-02-01--00-00-00"))

  # last rotation cache is set on creation
  bq <- BackupQueueDateTime$new(tf, cache_backups = FALSE)
  file.create(paste0(tools::file_path_sans_ext(tf), ".2019-03-01--00-00-00.log.zip"))
  expect_equal(bq$last_rotation, parse_datetime("2019-03-01--00-00-00"))

  expect_equal(bq$n, 3)
  bq <- BackupQueueDateTime$new(tf, cache_backups = TRUE)
  bq$prune(0)
  expect_null(bq$last_rotation)
})




test_that("BackupQueueDateTime can find and prune backup trails", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01T22-22-22.log.zip", ".20190102---1212.log.tar.gz", ".20190103174919.log", ".12.log"))
  file.create(bus)
  bq <- BackupQueueDateTime$new(tf)

  expect_identical(
    bq$files$timestamp,
    as.POSIXct(c("2019-01-03 17:49:19", "2019-01-02 12:12:00", "2019-01-01 22:22:22"))
  )

  # backup_matrix stays a matrix even if it has only one row
  bq$prune(1)
  expect_identical(bq$n, 1L)

  bq <- BackupQueue$new(tf)
  bq$prune(0)
  bq$prune(0)
  expect_identical(bq$n, 0L)
  file.remove(tf)
})




test_that("BackupQueueDatetime works with supported timestamp formats", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  datetime <- as.POSIXct("2019-01-01 00:00:00")

  bq <- BackupQueueDateTime$new(tf)
  expect_identical(bq$n, 0L)
  for (i in 1:10) {
    bq$push(now = datetime + i * 5)
  }
  bq$prune(5)
  expect_length(bq$files$path, 5)

  file.create(file.path(td, "test.2019-02-20.log"))
  file.create(file.path(td, "test.2019-04-20T12-00-00.log"))
  file.create(file.path(td, "test.2019-04-20T120000.log"))
  file.create(file.path(td, "test.2019-04-20T1200.log"))
  file.create(file.path(td, "test.2019-04-20--12.log"))
  file.create(file.path(td, "test.20190420T12.log"))
  bq$update_backups_cache()

  bq$prune(7)

  eres <- c(
    "2019-04-20 12:00:00", "2019-04-20 12:00:00", "2019-04-20 12:00:00",
    "2019-04-20 12:00:00", "2019-04-20 12:00:00", "2019-02-20 00:00:00",
    "2019-01-01 00:00:50"
  )

  expect_identical(
    as.character(bq$files$timestamp),
    eres
  )

  bq$files$timestamp
  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)

  expect_identical(
    as.character(bq$files$timestamp),
    eres
  )

  bq$prune(0)
})



test_that("Prune BackupQueueDate based on date", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-01-02--12-12-12.log.tar.gz",
    ".2019-01-03.log",
    ".2020-01-03.log"
  ))
  file.create(bus)

  bq <- BackupQueueDateTime$new(tf)
  bq$prune(as.Date("2019-01-02"))

  expect_identical(
    basename(bq$files$path),
    c(
      "test.2020-01-03.log",
      "test.2019-01-03.log",
      "test.2019-01-02--12-12-12.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on year interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01--12-12-12.log.zip",
    ".2019-02-02--12-12-12.log.tar.gz",
    ".2019-03-03--12-12-12.log",
    ".2020-01-03--12-12-12.log",
    ".2021-01-03--12-12-12.log",
    ".2022-01-03--12-12-12.log"
  ))
  file.create(bus)
  bq <- BackupQueueDateTime$new(tf)

  bq$prune("2 years")
  expect_identical(
    basename(bq$files$path),
    c(
      "test.2022-01-03--12-12-12.log",
      "test.2021-01-03--12-12-12.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on month interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01--00-00-00.log.zip",
    ".2019-02-02--00-00-00.log.tar.gz",
    ".2019-03-03--00-00-00.log",
    ".2019-04-03--00-00-00.log"
  ))
  file.create(bus)

  bq <- BackupQueueDateTime$new(tf)
  bq$prune("2 months")
  expect_identical(
    basename(bq$files$path),
    c(
      "test.2019-04-03--00-00-00.log",
      "test.2019-03-03--00-00-00.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on week interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-15--00-00-00.log",
    ".2019-04-08--00-00-00.log.tar.gz",
    ".2019-04-07--00-00-00.log.zip"
  ))
  file.create(bus)

  bq <- BackupQueueDateTime$new(tf)
  bq$prune("2 weeks")
  expect_identical(
    basename(bq$files$path),
    c(
      "test.2019-04-15--00-00-00.log",
      "test.2019-04-08--00-00-00.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on days interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-07--00-00-00.log.zip",
    ".2019-04-08--00-00-00.log.tar.gz",
    ".2019-04-09--00-00-00.log"
  ))
  file.create(bus)

  bq <- BackupQueueDateTime$new(tf)
  bq$prune("2 days")
  expect_identical(
    basename(bq$files$path),
    c(
      "test.2019-04-09--00-00-00.log",
      "test.2019-04-08--00-00-00.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("BackupQueueDate $last_date", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDateTime$new(tf, cache_backups = FALSE)

  expect_identical(bq$n, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01--00-00-00.log.zip", ".2019-01-02--00-00-00.log.tar.gz", ".2019-01-03--00-00-00.log"))
  file.create(bus)
  expect_identical(bq$files$sfx, c("2019-01-03--00-00-00", "2019-01-02--00-00-00", "2019-01-01--00-00-00"))
  expect_equal(bq$last_rotation, as.POSIXct("2019-01-03--00-00-00"))

  bq$prune(0)
  file.remove(tf)
})




test_that("BackupQueueDateTime$push() can push to different directory", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  tf <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  file.create(tf)
  on.exit(unlink(c(bu_dir, tf), recursive = TRUE), add = TRUE)


  bt <- BackupQueueDateTime$new(tf, dir = bu_dir)
  bt$push()

  expect_match(dirname(bt$files$path), "rotor.backups")
  bt$set_compression(TRUE)
  bt$push()

  expect_identical(bt$n, 2L)

  expect_length(bt$prune(0)$files$path, 0)
  expect_length(list.files(bu_dir), 0)
})



test_that("BackupQueueDateTime: $should_rotate", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  bq <- BackupQueueDateTime$new(tf)
  bq$push(now = "2019-01-01")

  expect_false(bq$should_rotate("0.5kb", age = "1 year", now = "2019-12-31"))
  expect_true(bq$should_rotate("0.5kb", age = "1 year", now = "2020-01-01"))
  expect_true(bq$should_rotate("0.5kb", age = "0 year"))
})




test_that("BackupQueueDateTime: `age` works with no backups", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)

  now <- as.Date(as.character(file.info(tf)$ctime))
  bq <- BackupQueueDateTime$new(tf)
  expect_false(bq$should_rotate(age = "10 day", now = now, size = 0))
  expect_true(bq$should_rotate(age = "0 day", now = now, size = 0))
})





test_that("BackupQueueDateTime: $should_rotate(verbose = TRUE) displays helpful messages", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)
  bq <- BackupQueueDateTime$new(tf)

  expect_message(bq$should_rotate(age = Inf, size = "1 tb", verbose = TRUE), "age.*size")
  expect_message(bq$should_rotate(age = 1, size = "1 tb", verbose = TRUE), "size")
  expect_message(bq$should_rotate(age = "9999 years", size = -1, verbose = TRUE), "9999")
  expect_message(bq$should_rotate(age = "1000-12-31", size = -1, verbose = TRUE), "1000")
})




# BackupQueueDate ---------------------------------------------------------
context("BackupQueueDate")




test_that("BackupQueueDate: $set_max_backups", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)

  bq <- BackupQueueDateTime$new(tf)

  bq$set_max_backups(1)
  expect_identical(bq$max_backups, 1L)

  bq$set_max_backups("1 day")
  expect_s3_class(bq$max_backups, "rotation_interval")
  expect_identical(bq$max_backups$value, 1L)
  expect_identical(bq$max_backups$unit, "day")

  bq$set_max_backups("2019-01-01")
  expect_identical(bq$max_backups, as.Date("2019-01-01"))

  bq$set_max_backups(as.Date("2019-01-01"))
  expect_identical(bq$max_backups, as.Date("2019-01-01"))
})




test_that("BackupQueueDate can find and prune backup trails", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf)

  expect_identical(bq$n, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01.log.zip", ".2019-01-02.log.tar.gz", ".2019-01-03.log", ".12.log"))
  file.create(bus)
  bq$update_backups_cache()
  expect_identical(bq$files$sfx, c("2019-01-03", "2019-01-02", "2019-01-01"))

  # backup_matrix stays a matrix even if it has only one row
  bq$prune(1)
  expect_identical(bq$n, 1L)

  bq <- BackupQueue$new(tf)
  bq$prune(0)
  bq$prune(0)
  expect_identical(bq$n, 0L)
  file.remove(tf)
})



test_that("BackupQueueDate works with supported datestamp formats", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  date <- as.Date("2019-01-01")

  bq <- BackupQueueDate$new(tf, cache_backups = FALSE)
  expect_identical(bq$n, 0L)
  for (i in 1:10) {
    bq$push(now = as.POSIXct(date + i * 5))
  }
  bq$prune(5)
  expect_length(bq$files$path, 5)

  file.create(file.path(td, "test.2019-02-20.log"))
  file.create(file.path(td, "test.2019-04.log"))
  file.create(file.path(td, "test.2019.log"))
  file.create(file.path(td, "test.20200411.log"))
  file.create(file.path(td, "test.201905.log"))
  bq$prune(5)

  expect_length(bq$files$path, 5)

  noback <- file.path(dirname(tf), ".2019-a2-20.log")
  file.create(noback)

  expect_path_equal(
    basename(bq$files$path),
    c(
      "test.20200411.log",
      "test.201905.log",
      "test.2019-04.log",
      "test.2019-02-20.log",
      "test.2019.log"
    )
  )
})



test_that("Prune BackupQueueDate based on date", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-01-02.log.tar.gz",
    ".2019-01-03.log",
    ".2020-01-03.log"
  ))
  file.create(bus)
  bq <- BackupQueueDate$new(tf)
  bq$prune(as.Date("2019-01-02"))

  expect_path_equal(
    basename(bq$files$path),
    c(
      "test.2020-01-03.log",
      "test.2019-01-03.log",
      "test.2019-01-02.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on year interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-02-02.log.tar.gz",
    ".2019-03-03.log",
    ".2020-01-03.log",
    ".2021-01-03.log",
    ".2022-01-03.log"
  ))
  file.create(bus)

  bq <- BackupQueueDate$new(tf)
  bq$prune("2 years")
  expect_identical(
    basename(bq$files$path),
    c(
      "test.2022-01-03.log",
      "test.2021-01-03.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on month interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-01-01.log.zip",
    ".2019-02-02.log.tar.gz",
    ".2019-03-03.log",
    ".2019-04-03.log"
  ))
  file.create(bus)

  bq <- BackupQueueDate$new(tf)
  bq$prune("2 months")
  expect_path_equal(
    basename(bq$files$path),
    c(
      "test.2019-04-03.log",
      "test.2019-03-03.log"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on week interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-15.log",
    ".2019-04-08.log.tar.gz",
    ".2019-04-07.log.zip"
  ))
  file.create(bus)

  bq <- BackupQueueDate$new(tf)
  bq$prune("2 weeks")
  expect_path_equal(
    basename(bq$files$path),
    c(
      "test.2019-04-15.log",
      "test.2019-04-08.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("Prune BackupQueueDate based on days interval", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bus <- paste0(tools::file_path_sans_ext(tf), c(
    ".2019-04-07.log.zip",
    ".2019-04-08.log.tar.gz",
    ".2019-04-09.log"
  ))
  file.create(bus)

  bq <- BackupQueueDate$new(tf)
  bq$prune("2 days")
  expect_path_equal(
    basename(bq$files$path),
    c(
      "test.2019-04-09.log",
      "test.2019-04-08.log.tar.gz"
    )
  )
  bq$prune(0)
  file.remove(tf)
})




test_that("BackupQueueDate $last_rotation", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  bq <- BackupQueueDate$new(tf, cache_backups = FALSE)

  expect_identical(bq$n, 0L)
  bus <- paste0(tools::file_path_sans_ext(tf), c(".2019-01-01.log.zip", ".2019-01-02.log.tar.gz", ".2019-01-03.log"))
  file.create(bus)
  expect_identical(bq$files$sfx, c("2019-01-03", "2019-01-02", "2019-01-01"))

  expect_equal(bq$last_rotation, as.Date("2019-01-03"))

  bq$prune(0)
  file.remove(tf)
})




test_that("BackupQueueDateTime$push() can push to different directory", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  tf <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")
  dir.create(bu_dir)
  file.create(tf)
  on.exit(unlink(c(bu_dir, tf), recursive = TRUE), add = TRUE)


  bt <- BackupQueueDate$new(tf, dir = bu_dir)
  bt$push()

  expect_match(dirname(bt$files$path), "rotor.backups")
  bt$set_compression(TRUE)
  bt$push()

  expect_identical(bt$n, 2L)
  expect_length(bt$prune(0)$files$path, 0)
  expect_length(list.files(bu_dir), 0)
})




test_that("BackupQueueDate: $should_rotate", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  saveRDS(iris, tf)

  bq <- BackupQueueDate$new(tf)
  bq$push(now = "2019-01-01")

  expect_false(bq$should_rotate("0.5kb", age = "1 year", now = "2019-12-31"))
  expect_true(bq$should_rotate("0.5kb", age = "1 year", now = "2020-01-01"))
  expect_true(bq$should_rotate("0.5kb", age = "0 year"))
})




test_that("BackupQueueDate: backups_cache", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test.log")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)
  bq <- BackupQueueDate$new(tf, cache_backups = TRUE)
  file.create(paste0(tools::file_path_sans_ext(tf), ".2019-01-01T22-22-22.log.zip"))

  # last rotation cache must be updated manually
  expect_null(bq$last_rotation)
  bq$update_backups_cache()
  expect_equal(bq$last_rotation, parse_date("2019-01-01"))

  # last rotation cache is set on creation
  bq <- BackupQueueDate$new(tf, cache_backups = TRUE)
  expect_equal(bq$last_rotation, parse_date("2019-01-01"))

  # last rotation cache is updated on backup push
  bq <- BackupQueueDate$new(tf, cache_backups = TRUE)
  bq$push(now = "2019-02-01--00-00-00")
  expect_equal(bq$last_rotation, parse_date("2019-02-01"))

  # don't cache if set to FALSE
  bq <- BackupQueueDate$new(tf, cache_backups = FALSE)
  file.create(paste0(tools::file_path_sans_ext(tf), ".2019-03-01--00-00-00.log.zip"))
  expect_equal(bq$last_rotation, parse_date("2019-03-01"))

  # last rotation cache is set on prune
  expect_equal(bq$n, 3)
  bq <- BackupQueueDate$new(tf, cache_backups = TRUE)
  bq$prune(0)
  expect_null(bq$last_rotation)
})




test_that("BackupQueueDateTime: `age` works with no backups", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)

  now <- as.Date(as.character(file.info(tf)$ctime))
  bq <- BackupQueueDate$new(tf)
  expect_false(bq$should_rotate(age = "10 day", now = now, size = 0))
  expect_true(bq$should_rotate(age = "0 day", now = now, size = 0))
})




test_that("BackupQueueDate: $should_rotate(verbose = TRUE) displays helpful messages", {
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE))

  tf <- file.path(td, "test")
  file.create(tf)
  on.exit(unlink(tf), add = TRUE)
  bq <- BackupQueueDate$new(tf)

  expect_message(bq$should_rotate(age = Inf, size = "1 tb", verbose = TRUE), "age.*size")
  expect_message(bq$should_rotate(age = 1, size = "1 tb", verbose = TRUE), "size")
  expect_message(bq$should_rotate(age = "9999 years", size = -1, verbose = TRUE), "9999")
  expect_message(bq$should_rotate(age = "1000-12-31", size = -1, verbose = TRUE), "1000")
})




# cleanup -----------------------------------------------------------------

test_that("BackupQueue: all cleanup succesfull", {
  expect_length(list.files(td), 0)
})

