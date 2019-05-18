#' An internal R6 class for managing backups
#'
#' **documentation under development**
#' `BackupQueue` and its subclasses are [R6::R6Class] for rotating files and
#' managing backup. They are the internal constructs on which [rotate()] and
#' co. are based. The `BackupQueue` constructor is exported for use by other
#' package developers and not intended for direct use. It is still experimental
#' and die API is sure to change.
#'
#' @name BackupQueue
NULL





# BackupQueue -------------------------------------------------------------

#' @rdname BackupQueue
#' @export
BackupQueue <- R6::R6Class(
  "BackupQueue",
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      max_backups = Inf,
      compression = FALSE
    ){
      self$file <- file
      self$backup_dir <- backup_dir
      self$compression <- compression
      self$max_backups <- max_backups

      self
    },

    file = NULL,
    backup_dir = NULL,
    max_backups = NULL,
    compression = NULL,

    prune = function(
      max_backups
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
    }
  ),


  active = list(
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
      fname_df     <- as.data.frame(fname_matrix, stringsAsFactors = FALSE)
      finfo <- file.info(backup_files)

      res <- cbind(
        data.frame(path = row.names(finfo), stringsAsFactors = FALSE),
        fname_df,
        finfo
      )
      row.names(res) <- NULL

      res
    }
  )
)


# BackupQueueIndex --------------------------------------------------------

#' @rdname BackupQueue
#' @export
BackupQueueIndex <- R6::R6Class(
  "BackupQueueIndex",
  inherit = BackupQueue,
  public = list(
    prune = function(
      max_backups
    ){
      if (!should_prune(self, max_backups))
        return(self)

      to_keep   <- self$backups$path[seq_len(max_backups)]
      to_remove <- setdiff(self$backups$path, to_keep)

      file_remove(to_remove)
      self$pad_index()
    },

    should_rotate = function(size){
      size <- parse_size(size)

      # try to avoid costly file.size check
      if (size <= 0)
        return(TRUE)

      if (is.infinite(size))
        return(FALSE)

      file.size(self$file) > size
    },

    push_backup = function(
      compression = FALSE
    ){
      assert_valid_compression(compression)
      self$increment_index()

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

      copy_or_compress(
        self$file,
        outname = name_new,
        compression = compression,
        add_ext = TRUE,
        overwrite = FALSE
      )

      self$pad_index( )
    },


    pad_index = function(
    ){
      if (nrow(self$backups) <= 0)
        return(self)

      backups <- self$backups
      backups$sfx_new <- pad_left(backups$index, pad = "0")
      backups$path_new <-
        paste(file.path(backups$dir, backups$name), backups$sfx_new, backups$ext, sep = ".")

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
      if (self$n_backups <= 0){
        return(self)
      }
      assert(is_scalar_integerish(n))

      backups <- self$backups

      backups$index <- backups$index + as.integer(n)
      backups$path_new <- paste(
        file.path(backups$dir, backups$name),
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

  active = list(
    backups = function(){
      res <- super$backups

      if (nrow(res) < 1){
        return(EMPTY_BACKUPS_INDEX)
      }
      res <- res[grep("^\\d+$", res$sfx), ]
      res$index <- as.integer(res$sfx)

      res[order(res$sfx, decreasing = FALSE), ]
    }
  )
)



# BackupQueueDateTime -----------------------------------------------------

#' @rdname BackupQueue
#' @export
BackupQueueDateTime <- R6::R6Class(
  "BackupQueueDateTime",
  inherit = BackupQueue,
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      max_backups = Inf,
      compression = FALSE,
      format = "%Y-%m-%d--%H-%M-%S",
      cache_last_rotation = TRUE
    ){
      assert(is_scalar_bool(cache_last_rotation))
      self$file <- file
      self$backup_dir <- backup_dir
      self$fmt <- format
      self$compression <- compression
      self$max_backups <- max_backups
      self$set_cache_last_rotation(cache_last_rotation)

      self
    },

    fmt = NULL,
    cache_last_rotation = NULL,

    push_backup = function(
      compression = FALSE,
      overwrite = FALSE,
      now = Sys.time()
    ){
      assert_valid_datetime_format(self$fmt)
      assert_valid_compression(compression)

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
        compression = compression,
        add_ext = TRUE,
        overwrite = overwrite
      )

      self$update_last_rotation_cache()
      self
    },

    should_rotate = function(
      size,
      age,
      now = Sys.time(),
      last_rotation = self$last_rotation
    ){
      now <- parse_datetime(now)
      size <- parse_size(size)

      # try to avoid costly file.size check
      if (is.infinite(size) || file.size(self$file) < size)
        return(FALSE)

      if (is.null(age) || is.null(self$last_rotation))
        return(TRUE)

      else if (is_parsable_datetime(age))
        return(is_backup_older_than_datetime(self$last_rotation, age))

      else if (is_parsable_interval(age))
        return(is_backup_older_than_interval(self$last_rotation, age, now))

      stop("`age` must be a parsable date or datetime")
    },

    update_last_rotation_cache = function(){
      bu <- self$backups
      private$last_rotation_cache <-
        if (!nrow(bu)) NULL else max(bu$timestamp)
      self
    },

    set_cache_last_rotation = function(x){
      assert(is_scalar_bool(x))
      private$.cache_last_rotation <- x
      self$update_last_rotation_cache()
    },

    prune = function(
      max_backups
    ){
      assert(is_scalar(max_backups))

      if (!should_prune(self, max_backups))
        return(self)


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

        } else if (is_parsable_interval(max_backups)){
          # interval like strings
          interval <- parse_interval(max_backups)

          last_rotation <- as.Date(as.character(self$last_rotation))

          if (identical(interval[["unit"]], "year")){
            limit <- dint::first_of_year(dint::get_year(last_rotation) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "quarter")){
            limit <- dint::first_of_quarter(dint::as_date_yq(last_rotation) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "month")) {
            limit <- dint::first_of_month(dint::as_date_ym(last_rotation) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "week")){
            limit <- dint::first_of_isoweek(dint::as_date_yw(last_rotation) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "day")){
            limit <- as.Date(last_rotation) - interval$value + 1L
          }

          to_remove <- self$backups$path[as.Date(as.character(self$backups$timestamp)) < limit]
        }
      }

      file_remove(to_remove)
      self$update_last_rotation_cache()
      self
    }),

  active = list(

      last_rotation = function(){
      if (get(".cache_last_rotation", envir = private, mode = "logical")){
        get("last_rotation_cache", private)
      } else {
        max(self$backups$timestamp)
      }
    },

    backups = function(){
      res <- super$backups

      if (nrow(res) < 1){
        return(EMPTY_BACKUPS_DATETIME)
      }

      sel <- vapply(res$sfx, is_parsable_datetime, logical(1))
      res <- res[sel, ]

      res$timestamp <- parse_datetime(res$sfx)
      res[order(res$timestamp, decreasing = TRUE), ]
    }
  ),

  private = list(
    last_rotation_cache = NULL,
    .cache_last_rotation = NULL
  )
)



# BackupQueueDate ---------------------------------------------------------

#' @rdname BackupQueue
#' @export
BackupQueueDate <- R6::R6Class(
  inherit = BackupQueueDateTime,
  "BackupQueueDate",
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      format = "%Y-%m-%d",
      cache_last_rotation = TRUE
    ){
      self$file <- file
      self$backup_dir <- backup_dir
      self$fmt <- format
      self$set_cache_last_rotation(cache_last_rotation)

      self$update_last_rotation_cache()
      self
    },

    update_last_rotation_cache = function(){
      bu <- self$backups

      private$last_rotation_cache <-
        if (!nrow(bu)) NULL else as.Date(as.character(max(self$backups$timestamp)))

      self
    }
  ),

  active = list(
    last_rotation = function(){
      if (get(".cache_last_rotation", envir = private, mode = "logical")){
        get("last_rotation_cache", private)
      } else {
        as.Date(as.character(max(self$backups$timestamp)))
      }
    }
  )
)




# utils -------------------------------------------------------------------

parse_interval <- function(x){
  assert(is_scalar(x) && !is.na(x))

  if (is_integerish(x)){
    return(
      list(value = as.integer(x), unit = "day")
    )
  } else {
    assert(is.character(x))
  }

  splt <- strsplit(x, "\\s")[[1]]
  assert(identical(length(splt), 2L))

  value <- splt[[1]]
  unit  <- splt[[2]]

  valid_units <- c("day", "week", "month", "quarter", "year")
  unit <- gsub("s$", "", tolower(trimws(unit)))

  assert(unit %in% valid_units)

  list(value = as.integer(value), unit = unit)
}




filenames_as_matrix <- function(
  file,
  backups
){
  if (length(backups) < 1){
    return(NULL)
  }

  file_dir  <- dirname(file)
  file_name <- basename(tools::file_path_sans_ext(file))
  file_ext  <- tools::file_ext(file)

  back_dir  <- dirname(backups)
  assert(
    all_are_identical(back_dir),
    "All backups of `file` must be in the same directory, not \n",
    paste("*", unique(back_dir), collapse = "\n")
  )
  back_name <- basename(backups)

  filename_end <-
    attr(gregexpr(file_name, back_name[[1]])[[1]], "match.length") + 1L

  a <- strsplit_at_seperator_pos(back_name, filename_end)
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

  # compare tidy paths against each other, so tha we do not miss matches on
  # equivalent paths because of inconsistent path sepparators
  bu_dir <- path_tidy(as_scalar(dirname(potential_backups)))

  name <- basename(tools::file_path_sans_ext(file))
  ext  <- tools::file_ext(file)
  sfx_patterns <- paste0("(", sfx_patterns, ")", collapse = "|")

  path_pat <- {if (bu_dir %in% c(".", "")) "" else paste0(bu_dir, "[\\/\\\\]")}

  if (is_blank(ext)){
    pat <- sprintf("^%s%s\\.%s\\.*$", path_pat, name, sfx_patterns)

  } else {
    pat <- sprintf("^%s%s\\.%s\\.%s\\.*$", path_pat, name, sfx_patterns, ext)
  }

  # compare tidy paths, but return original paths
  sel <- grep(pat, path_tidy(potential_backups))
  sort(potential_backups[sel])
}




EMPTY_BACKUPS <- data.frame(
  path = character(0),
  dir = character(0),
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
