# BackupQueue -------------------------------------------------------------

#' An R6 Class for managing backups (abstract base class)
#'
#' @description
#' `BackupQueue`is an abstract class not intended for direct usage, please refer to
#' [BackupQueueIndex], [BackupQueueDateTime], [BackupQueueDate] instead.
#'
#' @template r6_api
#'
#' @field dir `character` scalar. Directory in which to place the backups.
#' @field n `integer` scalar. The number of backups that exist for `BackupQueue$origin`
#'
#' @export
BackupQueue <- R6::R6Class(
  "BackupQueue",
  inherit = DirectoryQueue,
  cloneable = FALSE,
  public = list(
    initialize = function(
      origin,
      dir = dirname(origin),
      max_backups = Inf,
      compression = FALSE,
      # deprecated arguments
      backup_dir = NULL
    ){
      if (!is.null(backup_dir)){
        .Deprecated(msg = "the `backup_dir` argument is deprecated, please use `dir` instead.")
        dir <- backup_dir
      }

      self$set_origin(origin)
      self$set_dir(dir)
      self$set_compression(compression)
      self$set_max_backups(max_backups)

      self
    },


    #' @description Delete all backups except `max_backups`. See [prune_backups()].
    prune = function(
      max_backups = self$max_backups
    ){
      if (!should_prune(self, max_backups))
        return(self)

      if (max_backups > 0){ warning(
        "Pruning a generic BackupQueue with `max_backups > 0` is not",
        "recommended, because it is not defined which backups will be",
        "deleted. Use BackupQueueIndex or BackupQueueDate instead."
      )}

      if (self$n <= max_backups){
        to_remove <- character()
      } else {
        to_keep   <- self$files$path[seq_len(max_backups)]
        to_remove <- setdiff(self$files$path, to_keep)
      }

      file_remove(to_remove)
      self
    },



    #' @description Delete all identical backups. Uses [tools::md5sum()] to
    #'   compare the files.
    prune_identical = function(){
      NotImplementedError()
    },


    print = function(){
      cat(fmt_class(class(self)[[1]]), "\n\n")

      ori <- file.info(self$origin)
      bus <- self$files
      info <- data.frame(
        file = c(row.names(ori), bus$path),
        size = c(ori$size, bus$size)
      )
      dd <- as.matrix(info)

      if (nrow(dd) == 1){
        dd[, "size"] <- fmt_bytes(dd[, "size"])
        dd <- rbind(
          dd,
          c("[no backups]", "")
        )
        dd[, "size"] <- pad_left(dd[, "size"], max(nchar(dd[, "size"])) + 2)
        dd[, "file"] <- pad_right(dd[, "file"])
        assert(nrow(dd) == 2)
        dd[2, ] <-  apply(dd[2, ,drop = FALSE], 1:2, style_subtle)

      } else if (nrow(dd) > 1){
        dd <- rbind(
          dd,
          c(paste(nrow(dd), "files total"), sum(as.integer(dd[, "size"])))
        )
        dd[, "size"] <- pad_left(fmt_bytes(dd[, "size"]))
        dd[, "file"] <- pad_right(dd[, "file"])
        assert(nrow(dd) >= 3)
        sel <- 2:(nrow(dd) - 1)
        dd[sel, ] <-  apply(dd[sel, ,drop = FALSE], 1:2, style_subtle)

      } else {
        stop("Error while printing backup queue. Please file an issue.")
      }

      apply(dd, 1, cat, "\n")
      invisible(self)
    },

    # ... deprecated methods -----------
    push_backup = function(...){
      .Deprecated(new = "$push()", old = "$push_backup()")
      self$push(...)
    },


    # ... setters -------------------------------------------------------------
    #' Set the file to be backed up
    #' @param x a `character` scalar. Path to a file
    set_origin = function(
      x
    ){
      assert(
        is_scalar_character(x) && file_exists(x),
        "File '", x, "' does not exist"
      )
      assert(!is_dir(x))
      private[[".origin"]] <- x
      self
    },


    #' Set the file to be backed up
    #' @param x a `character` scalar. Path to a file
    set_compression = function(
      x
    ){
      assert_valid_compression(x)
      private[[".compression"]] <- x
      self
    },


    set_max_backups = function(
      x
    ){
      assert(
        is.infinite(x) || is_n0(x),
        "`max_backups` must be a positive integer (or `Inf` for no max)"
      )
      private[[".max_backups"]] <- x
      self
    },

    # ... deprecated setters ----------------------------------
    set_file = function(
      x
    ){
      .Deprecated(new = "$set_origin()", old = "$set_file()")
      self$set_origin(x)
    },

    set_backup_dir = function(
      x
    ){
      .Deprecated(new = "$set_dir()", old = "$set_backup_dir()")
      self$set_dir(x, create = FALSE)
    }
  ),




  # ... getters -------------------------------------------------------------
  active = list(
    #' @field file `character` scalar. The file to backup/rotate.
    origin = function(){
      get(".origin", envir = private)
    },


    #' @field compression (Optional) compression to use `compression` argument of [rotate()].
    compression = function(){
      get(".compression", envir = private)
    },

    #' @field max_backups Maximum number/size/age of backups. See `max_backups`
    #'   argument of [rotate()]
    max_backups = function(){
      get(".max_backups", envir = private)
    },

    #' @field has_backups Returns `TRUE` if at least one backup of `BackupQueue$origin`
    #'   exists
    has_backups = function(){
      self$n > 0
    },


    #' All backups of self$origin
    #' @return a `data.frame` with a similar structure to what
    #'   [base::file.info()] returns
    files = function(){
      backup_files <- get_backups(
        origin = self$origin,
        potential_backups =
          list_files(self$dir, full.names = self$dir != "."),
        sfx_patterns = c(
          "\\d+",
          "\\d{4}-\\d{2}-\\d{2}",
          "\\d{4}-\\d{2}"
        )
      )

      # parse to df
      if (!length(backup_files)){
        return(EMPTY_BACKUPS_INDEX)
      }

      fname_matrix <- filenames_as_matrix(self$origin, backups = backup_files)
      fname_df     <- as.data.frame(
        fname_matrix[, c("name", "sfx", "ext"), drop = FALSE],
        stringsAsFactors = FALSE
      )
      finfo <- file.info(backup_files)

      res <- cbind(
        path = data.frame(path = rownames(finfo), stringsAsFactors = FALSE),
        fname_df,
        finfo
      )
      row.names(res) <- NULL

      res
    },

# ... deprecated getters ------------------------------------------------

  backups = function(){
    .Deprecated(new = "$files", old = "$backups")
    self$files
  },

  file = function(){
    .Deprecated(new = "$origin", old = "$file")
    self$origin
  },

  backup_dir = function(){
    .Deprecated(new = "$dir", old = "$backup_dir")
    self$dir
  }
  ),

  private = list(
    .origin = NULL,
    .dir = NULL,
    .compression = NULL,
    .max_backups = NULL
  )
)


# BackupQueueIndex --------------------------------------------------------

#' An R6 class for managing indexed backups
#'
#' @description
#' A BackupQueue for indexed backups, e.g. `foo.log`, `foo.1.log`, `foo.2.log`, ...
#'
#' @template r6_api
#'
#' @export
BackupQueueIndex <- R6::R6Class(
  "BackupQueueIndex",
  inherit = BackupQueue,
  cloneable = FALSE,
  public = list(


    #' @description Create a new index-stamped backup (e.g. \file{logfile.1.log})
    push = function(){
      # generate new filename
        name <- file.path(
          self$dir,
          tools::file_path_sans_ext(basename(self$origin))
        )
        ext  <- tools::file_ext(self$origin)
        sfx <- "1"
        if (is_blank(ext)) {
          name_new <- paste(name, sfx, sep = ".")
        } else {
          name_new <- paste(name, sfx, ext, sep = ".")
        }

      self$increment_index()

      copy_or_compress(
        self$origin,
        outname = name_new,
        compression = self$compression,
        add_ext = TRUE,
        overwrite = FALSE
      )

      self$pad_index( )
    },


    prune = function(
      max_backups = self$max_backups
    ){
      if (!should_prune(self, max_backups))
        return(self)

      if (self$n > max_backups){
        to_keep   <- self$files$path[seq_len(max_backups)]
        to_remove <- setdiff(self$files$path, to_keep)
        file_remove(to_remove)
      }

      self$pad_index()
    },


    prune_identical = function(
    ){
      dd <- self$files
      dd$md5 <- tools::md5sum(self$files$path)

      dd <- dd[nrow(dd):1L, ]
      sel <- duplicated(dd$md5)

      remove <- dd[sel,  ]
      keep   <- dd[!sel, ]

      unlink(remove$path)

      keep$path_new <- paste(
        file.path(dirname(keep$path), keep$name),
        pad_left(nrow(keep):1, pad = "0"),
        keep$ext,
        sep = "."
      )
      keep$path_new <- gsub("\\.$", "", keep$path_new)

      # path_new will always have identical or lower indices than the old path.
      # if we sort be sfx we can prevent race conditions in file.rename
      # (i.e. where files would be overwriten because)
      keep <- keep[order(keep$sfx), ]
      file.rename(keep$path, keep$path_new)
      self
    },


    #' @description Should a file of `size` be rotated? See `size` argument of [`rotate()`]
    #' @return `TRUE` or `FALSE`
    should_rotate = function(size, verbose = FALSE){
      size <- parse_size(size)

      # try to avoid costly file.size check
      if (size <= 0)
        return(TRUE)

      if (is.infinite(size)){
        if (verbose) message("Not rotating: rotation `size` is infinite")
        return(FALSE)
      }

      fsize <- file.size(self$origin)

      if (fsize < size){
        if (verbose){
          message(sprintf(
            "Not rotating: size of '%s'(%s) is smaller than %s.",
            self$origin, fmt_bytes(file.size(self$origin)), fmt_bytes(size)
          ))
        }
        FALSE
      } else {
        TRUE
      }
    },


    #' @description Pad the indices in the filenames of indexed backups
    #'  to the number of digits of the largest index. Usually does not have to
    #'  be called manually.
    pad_index = function(){
      if (nrow(self$files) <= 0)
        return(self)

      backups <- self$files
      backups$sfx_new <- pad_left(backups$index, pad = "0")
      backups$path_new <-
        paste(file.path(dirname(backups$path), backups$name), backups$sfx_new, backups$ext, sep = ".")

      backups$path_new <- gsub("\\.$", "", backups$path_new)

      file_rename(
        backups$path,
        backups$path_new
      )

      self
    },


    #' @description Increment die Indices of all backups by `n` Usually does
    #' not have to be called manually.
    #'
    #' @param n `integer` > 0
    increment_index = function(
      n = 1
    ){
      assert(
        is_scalar_integerish(n) & n > 0,
        "indices can only be incremented by positive integers, but `n` is ", preview_object(n), "."
      )
      if (self$n <= 0)
        return(self)

      backups <- self$files
      backups$index <- backups$index + as.integer(n)
      backups$path_new <- paste(
        file.path(dirname(backups$path), backups$name),
        pad_left(backups$index, pad = "0"),
        backups$ext,
        sep = "."
      )
      backups$path_new <- gsub("\\.$", "", backups$path_new)

      file_rename(
        rev(backups$path),
        rev(backups$path_new)
      )

      self
    }
  ),


  # ... getters -------------------------------------------------------------
  active = list(

    files = function(){
      res <- super$files
      if (nrow(res) < 1)
        return(EMPTY_BACKUPS_INDEX)

      res <- res[grep("^\\d+$", res$sfx), ]
      res$index <- as.integer(res$sfx)
      res[order(res$sfx, decreasing = FALSE), ]
    }
  ),


  private = list(
    .fmt = NULL
  )
)



# BackupQueueDateTime -----------------------------------------------------

#' An R6 class for managing timestamped backups
#'
#' @description
#' A BackupQueue for timestamped backups, e.g. `foo.log`, `foo.2020-07-24_10-54-30.log`
#'
#' @template r6_api
#' @export
BackupQueueDateTime <- R6::R6Class(
  "BackupQueueDateTime",
  inherit = BackupQueue,
  cloneable = FALSE,
  public = list(
    initialize = function(
      origin,
      dir = dirname(origin),
      max_backups = Inf,
      compression = FALSE,
      fmt = "%Y-%m-%d--%H-%M-%S",
      cache_backups = FALSE,
      backup_dir = NULL
    ){
      if (!is.null(backup_dir)){
        .Deprecated(msg = "the `backup_dir` argument is deprecated, please use `dir` instead.")
        dir <- backup_dir
      }

      self$set_origin(origin)
      self$set_dir(dir)
      self$set_compression(compression)
      self$set_max_backups(max_backups)
      self$set_fmt(fmt)
      self$set_cache_backups(cache_backups)

      self$update_backups_cache()
    },

    #' @description Create a new time-stamped backup (e.g. \file{logfile.2020-07-22_12-26-29.log})
    #' @param overwrite `logical` scalar. Overwrite backups with the same
    #'   filename (i.e timestamp)?
    #' @param now `POSIXct` scalar. Can be used as an override mechanism for
    #'   the current system time if necessary.
    push = function(
      overwrite = FALSE,
      now = Sys.time()
    ){
      assert_valid_datetime_format(self$fmt)
      now <- parse_datetime(now)

      stopifnot(
        is_scalar_logical(overwrite),
        is_scalar_POSIXct(now)
      )

      # generate new filename
      name <- file.path(
        self$dir,
        tools::file_path_sans_ext(basename(self$origin))
      )

      ext  <- tools::file_ext(self$origin)
      sfx  <- format(now, format = self$fmt)

      if (is_blank(ext)) {
        name_new <- paste(name, sfx, sep = ".")
      } else {
        name_new <- paste(name, sfx, ext, sep = ".")
      }

      copy_or_compress(
        self$origin,
        outname = name_new,
        compression = self$compression,
        add_ext = TRUE,
        overwrite = overwrite
      )

      self$update_backups_cache()
    },


    prune = function(
      max_backups = self$max_backups
    ){
      assert(is_scalar(max_backups))

      if (!should_prune(self, max_backups)){
        return(self)
      } else {
        # to be safe
        self$update_backups_cache()
      }

      if (is_integerish(max_backups) && is.finite(max_backups)){
        # prune based on number of backups
        backups   <- rev(sort(self$files$path))

        if (length(backups) <= max_backups){
          to_remove <- character()
        } else {
          to_remove <- backups[(max_backups + 1):length(backups)]
        }

      } else {
        to_remove <- select_prune_files_by_age(
          self$files$path,
          self$files$timestamp,
          max_age = max_backups,
          now = as.Date(as.character(self$last_rotation))
        )
      }

      file_remove(to_remove)
      self$update_backups_cache()
      self
    },



    #' @description
    #' Should a file of `size` and `age` be rotated? See `size` and `age`
    #' arguments of [`rotate_date()`]. `now` overrides the current system time,
    #' `last_rotation`` overrides the date of the last rotation.
    #'
    #' @return `TRUE` or `FALSE`
    should_rotate = function(
      size,
      age,
      now = Sys.time(),
      last_rotation = self$last_rotation %||% file.info(self$origin)$ctime,
      verbose = FALSE
    ){
      now  <- parse_datetime(now)
      size <- parse_size(size)

      # try to avoid costly file.size check
      if (is.infinite(size) || is.infinite(age) || is.null(last_rotation) || file.size(self$origin) < size){
        if (verbose){
            reasons <- character()

            if (is.infinite(age))
              reasons[["age"]] <- "rotation `age` is infinite"

            if (is.infinite(size)){
              reasons[["size"]] <- "rotation `size` is infinite"

            } else if (file.size(self$origin) < size) {
              reasons[["size"]] <- sprintf(
                "size of '%s'(%s) is smaller than %s.",
                self$origin, fmt_bytes(file.size(self$origin)), fmt_bytes(size)
              )
            }

          message("Not rotating: ", paste(reasons, collapse = ", "))
        }
        return(FALSE)
      }

      if (is.null(last_rotation))
        return(TRUE)

      else if (is_parsable_datetime(age))
        return(is_backup_older_than_datetime(last_rotation, age, verbose = verbose))

      else if (is_parsable_rotation_interval(age))
        return(is_backup_older_than_interval(last_rotation, age, now, verbose = verbose))

      stop("`age` must be a parsable date or datetime")
    },


    #' @description Force update of the backups cache (only if `$cache_backups == TRUE`).
    update_backups_cache = function(){
      res <- super$files

      if (nrow(res) < 1){
        res <- EMPTY_BACKUPS_DATETIME

      } else {
        sel <- vapply(res$sfx, is_parsable_datetime, logical(1))
        res <- res[sel, ]
        res$timestamp <- parse_datetime(res$sfx)
        res <- res[order(res$timestamp, decreasing = TRUE), ]
      }

      private[["backups_cache"]] <- res
      self
    },


    # ... setters --------------------------------------------------------------
    set_max_backups = function(
      x
    ){
      assert(is.infinite(x) || is_n0(x) || is.character(x) || is_Date(x))

      if (is.infinite(x)){
        # do nothing

      } else if (is.character(x)){
        if (is_parsable_rotation_interval(x)){
          x <- parse_rotation_interval(x)
        } else {
          x <- parse_date(x)
        }

      } else if (is_n0(x)){
        x <- as.integer(x)
      }

      private[[".max_backups"]] <- x
      self
    },


    set_fmt = function(x){
      assert_valid_datetime_format(x)
      private[[".fmt"]] <- x
      self
    },


    set_cache_backups = function(x){
      assert(is_scalar_bool(x))
      private$.cache_backups <- x
      self$update_backups_cache()
    }
  ),


  # ... getters -------------------------------------------------------------
  active = list(

    #' @field fmt See `format` argument of [rotate_date()]
    fmt = function(){
      get(".fmt", envir = private, mode = "character")
    },


    #' `logical` scalar. If `TRUE` (the default) the list of backups is cached,
    #' if `FALSE` it is read from disk every time this appender triggers.
    #' Caching brings a significant speedup for checking whether to rotate or
    #' not based on the `age` of the last backup, but is only safe if there are
    #' no other programs/functions interacting with the backups. This is only
    #' advantageous for high frequency file rotation (i.e. several times per
    #' second)
    cache_backups = function(){
      get(".cache_backups", envir = private, mode = "logical")
    },


    #' `POSIXct` scalar. Timestamp of the last rotation (the last backup)
    last_rotation = function() {
      bus <- get("files", envir = self)
      if (nrow(bus) < 1) {
        NULL
      } else {
        max(get("files", envir = self)$timestamp)
      }
    },


    files = function(){
      if (!get(".cache_backups", envir = private, mode = "logical")){
        self$update_backups_cache()
      }
      get("backups_cache", envir = private)
    }
  ),


  private = list(
    backups_cache = NULL,
    .cache_backups = NULL,
    .fmt = NULL
  )
)



# BackupQueueDate ---------------------------------------------------------

#' An R6 class for managing datestamped backups
#'
#' @description
#' A BackupQueue for date-stamped backups, e.g. `foo.log`, `foo.2020-07-24.log`
#'
#' @template r6_api
#' @export
BackupQueueDate <- R6::R6Class(
  inherit = BackupQueueDateTime,
  "BackupQueueDate",
  cloneable = FALSE,
  public = list(

    initialize = function(
      origin,
      dir = dirname(origin),
      max_backups = Inf,
      compression = FALSE,
      fmt = "%Y-%m-%d",
      cache_backups = FALSE,
      backup_dir = NULL
    ){
      if (!is.null(backup_dir)){
        .Deprecated(msg = "the `backup_dir` argument is deprecated, please use `dir` instead.")
        dir <- backup_dir
      }

      self$set_origin(origin)
      self$set_dir(dir)
      self$set_compression(compression)
      self$set_max_backups(max_backups)
      self$set_fmt(fmt)
      self$set_cache_backups(cache_backups)

      self$update_backups_cache()
    },


    # ... setters -------------------------------------------------------------
    set_fmt = function(x){
      assert_valid_date_format(x)
      private[[".fmt"]] <- x
      self
    }
  ),


  # ... getters -------------------------------------------------------------
  active = list(

    last_rotation = function() {
      bus <- get("files", envir = self)
      if (nrow(bus) < 1) {
        NULL
      } else {
        as.Date(as.character(max(get("files", envir = self)$timestamp)))
      }
    }
  )
)




# utils -------------------------------------------------------------------

filenames_as_matrix <- function(
  file,
  backups
){
  if (length(backups) < 1)
    return(NULL)

  file_dir  <- dirname(file)
  file_name <- basename(tools::file_path_sans_ext(file))
  file_ext  <- tools::file_ext(file)

  back_dir  <- dirname(backups)
  assert(
    all_are_identical(back_dir),
    "All backups of `file` must be in the same directory, not \n",
    paste("*", unique(back_dir), collapse = "\n")
  )
  back_names <- basename(backups)

  filename_end <-
    attr(gregexpr(file_name, back_names[[1]])[[1]], "match.length") + 1L

  a <- strsplit_at_seperator_pos(back_names, filename_end)
  assert(
    all_are_identical(a[, 1]),
    "All backups of `file` must have the same basename, not  \n",
    paste("*", unique(a[, 1]), collapse = "\n")
  )


  if (!is_blank(file_ext)){
    ext_start <- unlist(gregexpr(file_ext, a[, 2]))
    b <- strsplit_at_seperator_pos(a[, 2], ext_start - 1L)
    res <- cbind(back_dir, a[, 1], b)
    colnames(res) <- c("dir", "name", "sfx", "ext")
  } else {
    res <- cbind(back_dir, a, "")
    colnames(res) <- c("dir", "name", "sfx", "ext")
  }

  assert(is.matrix(res))
  res
}




#' @param origin `character` scalar: The base file.
#' @param potential_backups `chracter` vector: list of files that could
#'   potentially be backups for `file` (and follow the rotor naming convention)
#' @noRd
get_backups <- function(
  origin,
  potential_backups,
  sfx_patterns
){
  if (!length(potential_backups))
    return(character())

  sfx_patterns <- paste0("(", sfx_patterns, ")", collapse = "|")

  file_dir  <- dirname(origin)
  file_name <- basename(tools::file_path_sans_ext(origin))
  file_ext  <- tools::file_ext(origin)

  back_dir  <- dirname(potential_backups)
  assert(
    all_are_identical(back_dir),
    "All backups of `origin` must be in the same directory, not \n",
    paste("*", unique(back_dir), collapse = "\n")
  )
  back_names <- basename(potential_backups)

  sel <- grepl(paste0("^", file_name), back_names)
  backups    <- potential_backups[sel]
  back_names <- basename(backups)

  if (!length(backups))
    return(character())

  # most file systems just support 255 character filenames
  file_sufext <- substr(back_names, nchar(file_name) + 2L, 64000L)

  sfx <- gsub("\\..*", "", file_sufext)
  sfx <- standardize_datetime_stamp(sfx)
  sel <- grepl( "^\\d{1,14}$", sfx)

  # compare tidy paths, but return original paths
  sort(backups[sel])
}




select_prune_files_by_age <- function(
  path,
  timestamp,
  max_age,
  now
){
  assert(is.character(path))
  assert(is_POSIXct(timestamp))
  assert(is_Date(now))
  assert(is_equal_length(path, timestamp))

  if (is_parsable_date(max_age)){
    limit     <- parse_date(max_age)
    to_remove <- path[as.Date(as.character(timestamp)) < limit]

  } else if (is_parsable_datetime(max_age)){
    limit     <- parse_datetime(max_age)
    to_remove <- path[timestamp < limit]

  } else if (is_parsable_rotation_interval(max_age)){
    max_age <- parse_rotation_interval(max_age)
    now <- as.Date(now)

    if (identical(max_age[["unit"]], "year")){
      limit <- dint::first_of_year(dint::get_year(now) - max_age$value + 1L)

    } else if (identical(max_age[["unit"]], "quarter")){
      limit <- dint::first_of_quarter(dint::as_date_yq(now) - max_age$value + 1L)

    } else if (identical(max_age[["unit"]], "month")) {
      limit <- dint::first_of_month(dint::as_date_ym(now) - max_age$value + 1L)

    } else if (identical(max_age[["unit"]], "week")){
      limit <- dint::first_of_isoweek(dint::as_date_yw(now) - max_age$value + 1L)

    } else if (identical(max_age[["unit"]], "day")){
      limit <- as.Date(as.character(now)) - max_age$value + 1L
    }

    to_remove <- path[as.Date(as.character(timestamp)) < limit]
  } else {
    stop(ValueError(paste0(preview_object(max_age), " is not a valid timestamp or interval. See ?rotate_time for more info.")))
  }
  to_remove
}




EMPTY_BACKUPS <- data.frame(
  path = character(0),
  name = character(0),
  sfx = character(0),
  ext = character(0),
  size = numeric(0),
  isdir = logical(0),
  mode = structure(integer(0)),
  mtime = structure(numeric(0), class = c("POSIXct", "POSIXt")),
  ctime = structure(numeric(0), class = c("POSIXct", "POSIXt")),
  atime = structure(numeric(0), class = c("POSIXct", "POSIXt")),
  uid = integer(0),
  gid = integer(0),
  uname = character(0),
  grname = character(0),
  stringsAsFactors = FALSE
)

EMPTY_BACKUPS_DATETIME <- EMPTY_BACKUPS
EMPTY_BACKUPS_DATETIME$timestamp <-
  structure(numeric(0), class = c("POSIXct", "POSIXt"), tzone = "")

EMPTY_BACKUPS_INDEX <- EMPTY_BACKUPS
EMPTY_BACKUPS_INDEX$index <- integer(0)
