context("Cache")


# generate lexically sortable ids. For equal timestamp, Cache$files is sorted
# by id, so that the tests do not fail on file systems with low-accuracy
# timestamps
.id_cache <- new.env()
assign("id", 0L, .id_cache)
ascending_id <- function(){
  x <- get("id", .id_cache)
  x <- pad_left(as.integer(x) + 1L, width = 8, pad = "0")
  assert(identical(nchar(x), 8L))
  assign("id", x, .id_cache)
  x
}


test_that("Cache works as expected", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # cache can be created
  cache <- Cache$new(td)

  # put elements into the cache
  key1 <- cache$push(iris)
  key2 <- cache$push(cars)
  expect_identical(cache$n, 2L)

  # read elements from the cache
  expect_identical(cache$read(key1), iris)
  expect_identical(cache$read(key2), cars)

  # remove
  cache$remove(key1)
  expect_identical(cache$n, 1L)
  expect_error(cache$read(key1))

  # pop
  expect_error(cache$pop(key1))
  res <- cache$pop(key2)
  expect_identical(cache$n, 0L)
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
  expect_identical(cache_hash$n, 1L)
  cache_hash$purge()
  expect_identical(cache_hash$n, 0L)


  # To override this behaviour use a function that generates globally unique ids instead of hashes
  cache_uid <- Cache$new(td, hashfun = function(x) ascending_id())
  cache_uid$push(iris)
  cache_uid$push(iris)
  expect_identical(cache_hash$n, 2L)
  cache_hash$purge()

  # fail if hashfun does not returns a scalar
  cache_err <- Cache$new(td, hashfun = function(x) c(ascending_id(),  ascending_id()))
  expect_error(cache_err$push(iris), class = "ValueError")
})





test_that("pruning works by number of files works", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  cache <- Cache$new(td, hashfun = function(x) ascending_id())
  k1 <- cache$push(iris)
  k2 <- cache$push(letters)
  k3 <- cache$push(cars)
  expect_identical(cache$n, 3L)

  # cached files are sorted in the order of their creation
  expect_identical(cache$files$key[[1]], k1)
  expect_identical(cache$files$key[[2]], k2)
  expect_identical(cache$files$key[[3]], k3)

  cache$prune(max_files = 2)
  expect_identical(cache$read(cache$files$key[[1]]), letters)
  expect_identical(cache$read(cache$files$key[[2]]), cars)
  cache$purge()
})



test_that("$files is ordered by key if timestamps are identical", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  cache <- Cache$new(td, hashfun = function(x) ascending_id())
  k1 <- cache$push(iris)
  k2 <- cache$push(letters)
  k3 <- cache$push(cars)
  expect_identical(cache$n, 3L)

  for (p in cache$files$path){  # loop necessary for compat with R < 3.6.0
    Sys.setFileTime(p, "1999-01-01 00:00:00")
  }

  expect_identical(cache$files$key[[1]], k1)
  expect_identical(cache$files$key[[2]], k2)
  expect_identical(cache$files$key[[3]], k3)

  cache$prune(max_files = 2)
  expect_identical(cache$read(cache$files$key[[1]]), letters)
  expect_identical(cache$read(cache$files$key[[2]]), cars)
  cache$purge()
})




test_that("pruning by size works", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # When using a real hash function as hashfun, identical objects will only
  # be added to the cache once
  cache <- Cache$new(td, hashfun = function(x) ascending_id())
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
  expect_identical(cache$n, 6L)

  expect_true(cache$size > 2048)
  cache$prune(max_size = "2kb")
  expect_true(cache$size <= 2048)

  cache$prune(max_files = 2)
  expect_identical(cache$read(cache$files$key[[2]]), cars)
  cache$purge
})




test_that("Inf max_* do not prunes", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # When using a real hash function as hashfun, identical objects will only
  # be added to the cache once
  cache <- Cache$new(td, hashfun = function(x) ascending_id())
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
  expect_identical(cache$n, 6L)

  cache$prune(max_files = Inf, max_age = Inf, max_size = Inf)
  expect_identical(cache$n, 6L)

  cache$prune(max_files = NULL, max_age = NULL, max_size = NULL)
  expect_identical(cache$n, 6L)

  cache$purge()
})




test_that("pruning by age works", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # create mock class that always
  MockCache <-  R6::R6Class(
    inherit = Cache,

    public = list(
      mock_timestamp = NULL
    ),

    active = list(
      files = function(){
        files <- list.files(self$dir, full.names = TRUE)

        if (!length(files)){
          return(EMPTY_CACHE_INDEX)
        }

        finfo <- file.info(files)

        res <- cbind(
          data.frame(path = rownames(finfo), stringsAsFactors = FALSE),
          data.frame(key = basename(rownames(finfo)), stringsAsFactors = FALSE),
          finfo
        )

        if (!is.null(self$mock_timestamp)){
          assert(length(self$mock_timestamp) >= nrow(res))
          res$atime <- res$ctime <- res$mtime <- self$mock_timestamp[1:nrow(res)]
        }

        row.names(res) <- NULL

        res[order(res$mtime), ]
      }
    )
  )

  cache <- MockCache$new(dir = td, hashfun = function(x) ascending_id())
  on.exit(cache$purge(), add = TRUE)
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

  expect_identical(nrow(cache$files), 5L)

  cache$mock_timestamp <- as.POSIXct(c(
    "2020-01-01",
    "2020-01-02",
    "2020-01-03",
    "2020-01-04",
    "2020-01-05"
  ))
  keep <- cache$files[cache$files$mtime >= as.POSIXct("2020-01-02"), ]
  expect_identical(
    nrow(keep),
    4L,
    label = paste0(nrow(keep), " (timestamps:", comma(keep$mtime), ")")
  )
  cache$prune(max_age = "2020-01-02")
  expect_setequal(cache$files$key, keep$key)
  cache$mock_timestamp <- as.POSIXct(c(
    "2020-01-02",
    "2020-01-03",
    "2020-01-04",
    "2020-01-05"
  ))

  keep <- cache$files[cache$files$mtime >= as.POSIXct("2020-01-04"), ]
  expect_identical(
    nrow(keep),
    2L,
    label = paste0(nrow(keep), " (timestamps:", comma(keep$mtime), ")")
  )
  cache$prune(max_age = "2 days", now = max(cache$files$mtime))
  expect_true(
    setequal(cache$files$key, keep$key),
    label = paste0(
      "[", paste(cache$files$mtime, cache$files$key, collapse = ", ", sep = ": "), "] == [",
      paste(keep$mtime, keep$key, collapse = " -- ", sep = ": "), "]"
    )
  )

  expect_error(
    cache$prune(max_age = "2 foos", now = max(cache$files$mtime)),
    class = "ValueError"
  )
})



test_that("$destroy works as expected", {
  td <- file.path(tempdir(), "cache-test")
  on.exit(unlink(td, recursive = TRUE))

  # cache can be created
  cache <- Cache$new(td)

  # put elements into the cache
  key1 <- cache$push(iris)
  key2 <- cache$push(cars)
  expect_identical(cache$n, 2L)

  expect_error(cache$destroy(), class = "DirIsNotEmptyError")
  cache$purge()$destroy()
  expect_false(dir.exists(cache$dir))
  expect_error(cache$push(iris), class = "DirDoesNotExistError")
})


