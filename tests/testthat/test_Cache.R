context("Cache")


test_that("Cache works as expected", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # cache can be created
  cache <- Cache$new(td)

  # put elements into the cache
  key1 <- cache$push(iris)
  key2 <- cache$push(cars)
  expect_identical(cache$n_files, 2L)

  # read elements from the cache
  expect_identical(cache$read(key1), iris)
  expect_identical(cache$read(key2), cars)

  # remove
  cache$remove(key1)
  expect_identical(cache$n_files, 1L)
  expect_error(cache$read(key1))

  # pop
  expect_error(cache$pop(key1))
  res <- cache$pop(key2)
  expect_identical(cache$n_files, 0L)
  expect_identical(res, cars)
})




test_that("setting hash functions work", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # When using a real hash function as hashfun, identical objects will only
  # be added to the cache once
  cache_hash <- Cache$new(td, hashfun = digest::digest)
  cache_hash$push(iris)
  cache_hash$push(iris)
  expect_identical(cache_hash$n_files, 1L)
  cache_hash$purge()
  expect_identical(cache_hash$n_files, 0L)


  # To override this behaviour use a generate for unique ids, such as
  cache_uid <- Cache$new(td, hashfun = function(x) uuid::UUIDgenerate())
  cache_uid$push(iris)
  cache_uid$push(iris)
  expect_identical(cache_hash$n_files, 2L)
  cache_hash$purge()

  # ensure hashfun allways returns a scalar
  cache_err <- Cache$new(td, hashfun = function(x) uuid::UUIDgenerate(n = 2))
  expect_error(cache_err$push(iris), "scalar")
})





test_that("pruning works", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # When using a real hash function as hashfun, identical objects will only
  # be added to the cache once
  cache <- Cache$new(td, hashfun = function(x) uuid::UUIDgenerate())
  k1 <- cache$push(iris)
  Sys.sleep(0.1)
  k2 <- cache$push(letters)
  Sys.sleep(0.1)
  k3 <- cache$push(cars)
  expect_identical(cache$n_files, 3L)

  cache$prune(max_files = 2)
  cache$files
  expect_identical(cache$read(cache$files$key[[1]]), letters)
  expect_identical(cache$read(cache$files$key[[2]]), cars)
  cache$purge()
})




test_that("pruning by size works", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # When using a real hash function as hashfun, identical objects will only
  # be added to the cache once
  cache <- Cache$new(td, hashfun = function(x) uuid::UUIDgenerate())
  cache$push(iris)
  Sys.sleep(0.1)
  cache$push(iris)
  Sys.sleep(0.1)
  cache$push(iris)
  Sys.sleep(0.1)
  cache$push(iris)
  Sys.sleep(0.1)
  cache$push(iris)
  Sys.sleep(0.1)
  cache$push(cars)
  expect_identical(cache$n_files, 6L)

  expect_true(cache$size > 2048)
  cache$prune(max_size = "2kb")
  cache$files
  expect_true(cache$size <= 2048)

  cache$prune(max_files = 2)
  expect_identical(cache$read(cache$files$key[[2]]), cars)
  cache$purge
})
