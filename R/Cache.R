# Cache ----------------------------------------------------------------

#' An R6 class for managing a persistent file-based cache
#'
#' @description
#' `Cache` provides an [R6][R6::R6Class] API for managing an on-disk key-value
#' store for \R objects. The objects are serialized to a single folder as
#' [.rds][readRDS()] files and the key of the object equals the name of the file.
#' `Cache` supports automatic removal of old files if the cache folder exceeds a
#' predetermined number of files, total size, or if the individual files exceed
#' a certain age.
#'
#' @template r6_api
#'
#' @field dir a `character` scalar. path of the directory in which to store the cache files
#' @field n `integer` scalar: number of files in the cache
#'
#' @export
Cache <- R6::R6Class(
  "Cache",
  cloneable = FALSE,
  inherit = DirectoryQueue,
  public = list(
  #'
  #' @param create_dir `logical` scalar. If `TRUE` `dir` is created if it
  #'   does not exist.
  #'
  #' @examples
  #' td <- file.path(tempdir(), "cache-test")
  #'
  #' # When using a real hash function as hashfun, identical objects will only
  #' # be added to the cache once
  #' cache_hash <- Cache$new(td, hashfun = digest::digest)
  #' cache_hash$push(iris)
  #' cache_hash$push(iris)
  #' cache_hash$files
  #' cache_hash$purge()
  #'
  #' # To override this behaviour use a generator for unique ids, such as uuid
  #' if (requireNamespace("uuid")){
  #'   cache_uid <- Cache$new(td, hashfun = function(x) uuid::UUIDgenerate())
  #'   cache_uid$push(iris)
  #'   cache_uid$push(iris)
  #'   cache_uid$files
  #'   cache_uid$purge()
  #' }
  #'
  #' unlink(td, recursive = TRUE)
    initialize = function(
      dir = dirname(file),
      max_files = Inf,
      max_size  = Inf,
      max_age   = Inf,
      compression = TRUE,
      hashfun  = digest::digest,
      create_dir = TRUE
    ){
      self$set_dir(dir, create = create_dir)
      self$set_max_files(max_files)
      self$set_max_size(max_size)
      self$set_max_age(max_age)
      self$set_hashfun(hashfun)
      self$set_compression(compression)
      self
    },

    #' @description push a new object to the cache
    #'
    #' @param x any \R object
    #' @param key a `character` scalar. Key under which to store the cached
    #'   object. Must be a valid filename. Defaults to being generated by
    #'   `$hashfun()` but may also be supplied manually.
    #'
    #' @return a `character` scalar: the key of the newly added object
    push = function(
      x,
      key = self$hashfun(x)
    ){
      assert(
        is_scalar(key),
        ValueError(paste0(
        "`key` must be a scalar, not ", preview_object(key), ". Did you set a",
        " custom `$hashfun` that can return vectors of length > 1?"
        ))
      )
      assert(dir.exists(self$dir), DirDoesNotExistError(dir = self$dir))

      saveRDS(x, file = file.path(self$dir, key), compress = self$compression)
      self$prune()
      key
    },

    #' @description read a cached file
    #' @param key `character` scalar. key of the cached file to read.
    read = function(
      key
    ){
      path <- file.path(self$dir, key)
      assert(
        file.exists(path),
        "'", key, "' does not exist in ", self$dir
      )
      readRDS(file.path(self$dir, key))
    },

    #' @description remove a single file from the cache
    #' @param key `character` scalar. key of the cached file to remove
    remove = function(
      key
    ){
      unlink(file.path(self$dir, key))
      invisible(NULL)
    },

    #' @description Read and remove a single file from the cache
    #' @param key `character` scalar. key of the cached file to read/remove
    pop = function(
      key
    ){
      res <- self$read(key)
      self$remove(key)
      res
    },

    #' @description Prune the cache
    #'
    #' Delete cached objects that match certain criteria. `max_files` and
    #' `max_size` deletes the oldest cached objects first; however, this is
    #' dependent on accuracy of the file modification timestamps on your system.
    #' For example, ext3 only supports second-accuracy, and some windows
    #' version only support timestamps at a resolution of two seconds.
    #'
    #' If two files have the same timestamp, they are deleted in the lexical
    #' sort order of their key. This means that by using a function that
    #' generates lexically sortable keys as `hashfun` (such as
    #' [ulid::generate()]) you can enforce the correct deletion order. There
    #' is no such workaround if you use a real hash function.
    #'
    #' @param max_files,max_size,max_age see section Active Bindings.
    #' @param now a `POSIXct` datetime scalar. The current time (for max_age)
    prune = function(
      max_files = self$max_files,
      max_size  = self$max_size,
      max_age   = self$max_age,
      now = Sys.time()
    ){
      assert(is.null(max_files) || is.infinite(max_files) || is_n0(max_files))
      files <- self$files
      files <- files[order(files$mtime), ]
      now <- as.Date(format(now))

      rem <- list()

      if (!is.null(max_age) && !is.infinite(max_age)){
        rem$age  <- list(path = select_prune_files_by_age(
          path = files$path,
          timestamp = files$mtime,
          max_age = max_age,
          now = now
        ))
      }

      if (!is.null(max_size) && !is.infinite(max_size)){
        max_size  <- parse_size(max_size)
        files$cumsize <- rev(cumsum(rev(files$size)))
        rem$size <- files[files$cumsize > max_size, ]
      }

      if (!is.null(max_files) && self$n > max_files){
        rem$n <- self$files[1L:(self$n - max_files),]
      }

      if (length(rem)){
        to_remove <- unlist(lapply(rem, `[[`, "path"), use.names = FALSE)
        file.remove(to_remove)
      }

      invisible(self)
    },

    #' @description purge the cache (remove all cached files)
    purge = function(
    ){
      unlink(self$files$path)
      invisible(self)
    },


    #' @description purge the cache (remove all cached files)
    destroy = function(
    ){
      files <- list.files(self$dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
      assert(!length(files), DirIsNotEmptyError(dir = self$dir))

      unlink(self$dir, recursive = TRUE)
      invisible(self)
    },


    print = function(){
      cat(fmt_class(class(self)[[1]]), "\n\n")

      cat(self$dir)

      if (length(self$files)){
        cat(style_subtle("\t[", self$n, " files; ", fmt_bytes(self$size), "]", sep = ""))
      } else {
        cat(style_subtle("\t[", fmt_bytes(self$size), "]", sep = ""))
      }


      invisible(self)
    },


    # ... setters -------------------------------------------------------------
    set_max_files = function(
      x
    ){
      if (is.infinite(x))
        x <- NULL

      assert(is.null(x) || is_n0(x))
      private[[".max_files"]] <- x
      invisible(self)
    },


    set_max_age = function(
      x
    ){
      if (!is.null(x))
        x <- NULL

      assert(is.null(x) || is_parsable_rotation_interval(x))
      private[[".max_age"]] <- x
      invisible(self)
    },


    set_max_size = function(
      x
    ){
      if (is.null(x))
        private[[".max_size"]] <- NULL
      else
        private[[".max_size"]] <- parse_size(x)

      invisible(self)
    },


    set_compression = function(
      x
    ){
      private[[".compression"]] <- x
      invisible(self)
    },

    set_hashfun = function(
      x
    ){
      assert(is.function(x) || is.null(x), "`hashfun` must be a function.")
      private[[".hashfun"]] <- x
      invisible(self)
    }
  ),


  # ... getters -------------------------------------------------------------
  active = list(
    #' @field max_files see the `compress` argument of [base::saveRDS()].
    #' **Note**: this differs from the `$compress` argument of [rotate()].
    compression = function(x){
      if (missing(x)) return(get(".compression", envir = private))
      self$set_compression(x)
    },


    #' @field max_files `integer` scalar: maximum number of files to keep in
    #' the cache
    max_n = function(x){
      if (missing(x)) return(get(".max_files", envir = private))
      self$set_max_files(x)
    },

    #' @field max_size scalar `integer`, `character` or `Inf`. Delete
    #'   cached files (starting with the oldest) until the total size of the
    #'   cache is below `max_size`. `Integers` are interpreted as bytes. You
    #'   can pass `character` vectors that contain a file size suffix like `1k`
    #'   (kilobytes), `3M` (megabytes), `4G` (gigabytes), `5T` (terabytes). Instead
    #'   of these short forms you can also be explicit and use the IEC suffixes
    #'   `KiB`, `MiB`, `GiB`, `TiB`. In Both cases `1` kilobyte is `1024` bytes, 1
    #'   `megabyte` is `1024` kilobytes, etc... .
    max_size = function(x){
      if (missing(x)) return(get(".max_size", envir = private))
      self$set_max_size(x)
    },

    #' @field max_age
    #' - a `Date` scalar: Remove all backups before this date
    #' - a `character` scalar representing a Date in ISO format (e.g. `"2019-12-31"`)
    #' - a `character` scalar representing an Interval in the form `"<number> <interval>"` (see [rotate()])
    max_age = function(x){
      if (missing(x)) return(get(".max_age", envir = private))
      self$set_max_age(x)
    },


    #' @field hashfun `NULL` or a `function` to generate a unique hash from the
    #'   object to be cached (see example). The hash *must* be a text string
    #'   that is a valid filename on the target system. If `$hashfun` is `NULL`,
    #'   a storage key must be supplied manually in `cache$push()`. If a new
    #'   object is added with the same key as an existing object, the existing
    #'   object will be overwritten without warning.
    hashfun = function(fun){
      if (missing(fun)){
        res <- get(".hashfun", envir = private)
        if (is.null(res))
          stop("$hashfun is `NULL`. Please supply the key manually or set an $hashfun.")
        return(res)
      }

      self$set_hashfun(fun)
    },


    #' All cached files
    #' @return a `data.frame` with a similar structure to what
    #'   [base::file.info()] returns
    files = function(){

      files <- list.files(self$dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)

      if (!length(files)){
        return(EMPTY_CACHE_INDEX)
      }

      finfo <- file.info(files)

      res <- cbind(
        data.frame(path = rownames(finfo), stringsAsFactors = FALSE),
        data.frame(key = basename(rownames(finfo)), stringsAsFactors = FALSE),
        finfo
      )
      row.names(res) <- NULL

      assert(!is.null(res$mtime))
      res[order(res$mtime, res$key), ]
    },

    size = function(){
      sum(self$files$size)
    }
  ),

  private = list(
    .file = NULL,
    .dir = NULL,
    .max_files = NULL,
    .max_size = NULL,
    .max_age = NULL,
    .compression = NULL,
    .max_backups = NULL,
    .hashfun = NULL
  )
)



EMPTY_CACHE_INDEX <-
  structure(
    list(
      path = character(0),
      key = character(0),
      size = numeric(0),
      isdir = logical(0),
      mode = structure(integer(0), class = "octmode"),
      mtime = structure(numeric(0), class = c("POSIXct", "POSIXt")),
      ctime = structure(numeric(0), class = c("POSIXct", "POSIXt")),
      atime = structure(numeric(0), class = c("POSIXct", "POSIXt")),
      uid = integer(0),
      gid = integer(0),
      uname = character(0),
      grname = character(0)
    ),
    row.names = integer(0),
    class = "data.frame"
  )
