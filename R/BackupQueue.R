#' An internal R6 class for managing backups
#'
#' **documentation under development**
#' `BackupQueue` and its subclasses are [R6::R6Class] for rotating files and
#' managing backup. They are the internal constructs on which [rotate()] and
#' co. are based. The `BackupQueue` constructor is exported for use by other
#' package developers and not intended for direct use.
#'
#' @name BackupQueue
NULL




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
