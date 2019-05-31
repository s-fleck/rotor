#' An R6 class for managing backups
#'
#' @description
#' `BackupQueue` & co are part of the [R6][R6::R6] API of **rotor**. They are
#' used internally by [rotate()] and related functions and are not designed
#' for interactive use. Rather, if you are a package developer and want to
#' integrate rotor in one of your package, the `BackupQueue` subclasses give
#' you a bit of extra control.
#'
#' As of now, **the R6 API is still experimental and subject to change**.
#'
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`pad_index()`}{Pad the indices in the filenames of indexed backups
#'     to the number of digits of the largest index. Usually does not have to
#'     be called manually.
#'   }
#'
#'   \item{`prune()`}{Delete all backups except `max_backups`. See [prune_backups()]}
#'
#'   \item{`push_backup() <BackupQueueIndex>`}{
#'     Create a new backup with index 1, push back all other indices.
#'     Always calls `$prune()` before it terminates.
#'   }
#'
#'   \item{`push_backup(overwrite = FALSE, now = Sys.time()) <BackupQueueDate> <BackupQueueDateteime>`}{
#'     Create a new backup with a timestamp. The `now` parameter override the
#'     real system time. If `overwrite` is `TRUE` existing backups with the
#'     same filename (i.e timestamp) are overwritten. Always calls
#'     `$prune()` before it terminates.
#'   }
#'
#'   \item{`backup_dir`, `set_backup_dir(x)`}{
#'     `character` scalar. Set a directory in which to place the backups
#'   }
#'
#'   \item{`cache_backups`, `set_cache_backups(x)`}{
#'     `TRUE` or `FALSE`. If `TRUE` (the default) the list of backups is cached,
#'     if `FALSE` it is read from disk every time this appender triggers.
#'     Caching brings a significant speedup for checking whether to rotate or
#'     not based on the `age` of the last backup, but is only safe if
#'     there are no other programs/functions (except this appender) interacting
#'     with the backups.
#'   }
#'
#'   \item{`compression`, `set_compression`}{See `compression` argument of [rotate()]}
#'
#'   \item{`file`, `set_file(x)`}{`character` scalar. The file to backup/rotate}
#'
#'   \item{`fmt`, `set_fmt(x)`}{
#'     `character` scalar. See `format` argument of [rotate_date()]
#'   }
#'
#'   \item{`max_backups`, `set_max_backups(x)`}{
#'     See `max_backups` argument of [rotate()]
#'   }
#'
#'   \item{`should_rotate(size) <BackupQueueIndex>`}{
#'     Should a file of `size` be rotated? See `size` argument of [`rotate()`]
#'   }
#'
#'   \item{`should_rotate(size, age, now = Sys.time(), last_rotation = self$last_rotation)  <BackupQueueDate> <BackupQueueDateteime>`}{
#'     Should a file of `size` and `age` be rotated? See `size` and `age`
#'     arguments of [`rotate_date()`]. `now` overrides the current system time,
#'     `last_rotation`` overrides the date of the last rotation.
#'   }
#'
#'   \item{`update_backups_cache()`}{
#'     Force update of the backups cache. Only does something if `$cache_backups`
#'     is `TRUE`.
#'   }
#' }
#'
#' @eval r6_usage(list(BackupQueueIndex, BackupQueueDate, BackupQueueDateTime))
#' @name BackupQueue
#' @aliases BackupQueueIndex BackupQueueDateTime BackupQueueDate
NULL




# BackupQueue -------------------------------------------------------------

#' @export
BackupQueue <- R6::R6Class(
  "BackupQueue",
  cloneable = FALSE,
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      max_backups = Inf,
      compression = FALSE
    ){
      self$set_file(file)
      self$set_backup_dir(backup_dir)
      self$set_compression(compression)
      self$set_max_backups(max_backups)

      self
    },


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
      to_keep   <- self$backups$path[seq_len(max_backups)]
      to_remove <- setdiff(self$backups$path, to_keep)
      file_remove(to_remove)

      self
    },


    print = function(){
      cat(fmt_class(class(self)[[1]]), "\n\n")

      ori <- file.info(self$file)
      bus <- self$backups
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


    # ... setters -------------------------------------------------------------
    set_file = function(
      x
    ){
      assert(is_scalar_character(x) && file_exists(x))
      assert(!is_dir(x))
      private[[".file"]] <- x
      self
    },


    set_backup_dir = function(
      x
    ){
      assert(
        is_scalar_character(x) && dir.exists(x),
        "backup dir '", x, "' does not exist."
      )
      private[[".backup_dir"]] <- x
      self
    },


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
      assert(is.infinite(x) || is_n0(x))
      private[[".max_backups"]] <- x
      self
    }
  ),


  # ... getters -------------------------------------------------------------
  active = list(
    file = function(){
      get(".file", envir = private)
    },

    backup_dir = function(){
      get(".backup_dir", envir = private)
    },

    compression = function(){
      get(".compression", envir = private)
    },

    max_backups = function(){
      get(".max_backups", envir = private)
    },

    has_backups = function(){
      self$n_backups > 0
    },


    n_backups = function(){
      nrow(self$backups)
    },


    backups = function(){
      backup_files <- get_backups(
        file = self$file,
        potential_backups =
          list_files(self$backup_dir, full.names = self$backup_dir != "."),
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

      fname_matrix <- filenames_as_matrix(self$file, backups = backup_files)
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
    }
  ),

  private = list(
    .file = NULL,
    .backup_dir = NULL,
    .compression = NULL,
    .max_backups = NULL
  )
)


# BackupQueueIndex --------------------------------------------------------

#' @export
BackupQueueIndex <- R6::R6Class(
  "BackupQueueIndex",
  inherit = BackupQueue,
  cloneable = FALSE,
  public = list(

    push_backup = function(){
      # generate new filename
        name <- file.path(
          self$backup_dir,
          tools::file_path_sans_ext(basename(self$file))
        )
        ext  <- tools::file_ext(self$file)
        sfx <- "1"
        if (is_blank(ext)) {
          name_new <- paste(name, sfx, sep = ".")
        } else {
          name_new <- paste(name, sfx, ext, sep = ".")
        }

      self$increment_index()

      copy_or_compress(
        self$file,
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

      to_keep   <- self$backups$path[seq_len(max_backups)]
      to_remove <- setdiff(self$backups$path, to_keep)

      file_remove(to_remove)
      self$pad_index()
    },


    should_rotate = function(size, verbose = FALSE){
      size <- parse_size(size)

      # try to avoid costly file.size check
      if (size <= 0)
        return(TRUE)

      if (is.infinite(size)){
        if (verbose) message("Not rotating: rotation `size` is infinite")
        return(FALSE)
      }

      fsize <- file.size(self$file)

      if (fsize < size){
        if (verbose){
          message(sprintf(
            "Not rotating: size of '%s'(%s) is smaller than %s.",
            self$file, fmt_bytes(file.size(self$file)), fmt_bytes(size)
          ))
        }
        FALSE
      } else {
        TRUE
      }
    },


    pad_index = function(){
      if (nrow(self$backups) <= 0)
        return(self)

      backups <- self$backups
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


    increment_index = function(
      n = 1
    ){
      assert(is_scalar_integerish(n))
      if (self$n_backups <= 0)
        return(self)

      backups <- self$backups
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
    backups = function(){
      res <- super$backups
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

#' @export
BackupQueueDateTime <- R6::R6Class(
  "BackupQueueDateTime",
  inherit = BackupQueue,
  cloneable = FALSE,
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      max_backups = Inf,
      compression = FALSE,
      fmt = "%Y-%m-%d--%H-%M-%S",
      cache_backups = FALSE
    ){
      self$set_file(file)
      self$set_backup_dir(backup_dir)
      self$set_compression(compression)
      self$set_max_backups(max_backups)
      self$set_fmt(fmt)
      self$set_cache_backups(cache_backups)

      self$update_backups_cache()
    },


    push_backup = function(
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
        self$backup_dir,
        tools::file_path_sans_ext(basename(self$file))
      )

      ext  <- tools::file_ext(self$file)
      sfx  <- format(now, format = self$fmt)

      if (is_blank(ext)) {
        name_new <- paste(name, sfx, sep = ".")
      } else {
        name_new <- paste(name, sfx, ext, sep = ".")
      }

      copy_or_compress(
        self$file,
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
        # to be save
        self$update_backups_cache()
      }

      if (is_integerish(max_backups) && is.finite(max_backups)){
        # prune based on number of backups
        backups   <- rev(sort(self$backups$path))
        to_remove <- backups[(max_backups + 1):length(backups)]

      } else {
        # prune based on dates and intervals
        if (is_parsable_date(max_backups)){
          limit     <- parse_date(max_backups)
          to_remove <- self$backups$path[as.Date(as.character(self$backups$timestamp)) < limit]

        } else if (is_parsable_datetime(max_backups)){
          limit     <- parse_datetime(max_backups)
          to_remove <- self$backups$path[self$backups$timestamp < limit]

        } else if (is_parsable_rotation_interval(max_backups)){
          max_backups <- parse_rotation_interval(max_backups)
          last_rotation <- as.Date(as.character(self$last_rotation))

          if (identical(max_backups[["unit"]], "year")){
            limit <- dint::first_of_year(dint::get_year(last_rotation) - max_backups$value + 1L)

          } else if (identical(max_backups[["unit"]], "quarter")){
            limit <- dint::first_of_quarter(dint::as_date_yq(last_rotation) - max_backups$value + 1L)

          } else if (identical(max_backups[["unit"]], "month")) {
            limit <- dint::first_of_month(dint::as_date_ym(last_rotation) - max_backups$value + 1L)

          } else if (identical(max_backups[["unit"]], "week")){
            limit <- dint::first_of_isoweek(dint::as_date_yw(last_rotation) - max_backups$value + 1L)

          } else if (identical(max_backups[["unit"]], "day")){
            limit <- as.Date(last_rotation) - max_backups$value + 1L
          }

          to_remove <- self$backups$path[as.Date(as.character(self$backups$timestamp)) < limit]
        } else {
          stop("Illegal `max_backups`")
        }
      }

      file_remove(to_remove)
      self$update_backups_cache()
      self
    },


    should_rotate = function(
      size,
      age,
      now = Sys.time(),
      last_rotation = self$last_rotation %||% file.info(self$file)$ctime,
      verbose = FALSE
    ){
      now  <- parse_datetime(now)
      size <- parse_size(size)

      # try to avoid costly file.size check
      if (is.infinite(size) || is.infinite(age) || is.null(last_rotation) || file.size(self$file) < size){
        if (verbose){
            reasons <- character()

            if (is.infinite(age))
              reasons[["age"]] <- "rotation `age` is infinite"

            if (is.infinite(size)){
              reasons[["size"]] <- "rotation `size` is infinite"

            } else if (file.size(self$file) < size) {
              reasons[["size"]] <- sprintf(
                "size of '%s'(%s) is smaller than %s.",
                self$file, fmt_bytes(file.size(self$file)), fmt_bytes(size)
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


    update_backups_cache = function(){
      res <- super$backups

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
    fmt = function(){
      get(".fmt", envir = private, mode = "character")
    },


    cache_backups = function(){
      get(".cache_backups", envir = private, mode = "logical")
    },


    last_rotation = function() {
      bus <- get("backups", envir = self)
      if (nrow(bus) < 1) {
        NULL
      } else {
        max(get("backups", envir = self)$timestamp)
      }
    },


    backups = function(){
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

#' @export
BackupQueueDate <- R6::R6Class(
  inherit = BackupQueueDateTime,
  "BackupQueueDate",
  cloneable = FALSE,
  public = list(

    initialize = function(
      file,
      backup_dir = dirname(file),
      max_backups = Inf,
      compression = FALSE,
      fmt = "%Y-%m-%d",
      cache_backups = FALSE
    ){
      self$set_file(file)
      self$set_backup_dir(backup_dir)
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
      bus <- get("backups", envir = self)
      if (nrow(bus) < 1) {
        NULL
      } else {
        as.Date(as.character(max(get("backups", envir = self)$timestamp)))
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




#' @param file `character` scalar: The base file.
#' @param potential_backups `chracter` vector: list of files that could
#'   potentially be backups for `file` (and follow the rotor naming convention)
#' @noRd
get_backups <- function(
  file,
  potential_backups,
  sfx_patterns
){
  if (!length(potential_backups))
    return(character())

  sfx_patterns <- paste0("(", sfx_patterns, ")", collapse = "|")

  file_dir  <- dirname(file)
  file_name <- basename(tools::file_path_sans_ext(file))
  file_ext  <- tools::file_ext(file)

  back_dir  <- dirname(potential_backups)
  assert(
    all_are_identical(back_dir),
    "All backups of `file` must be in the same directory, not \n",
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
