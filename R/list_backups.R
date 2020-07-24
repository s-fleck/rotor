#' Discover existing backups
#'
#' These function return information on the backups of a file (if any exist)
#'
#' @param file `character` scalar: Path to a file.
#' @inheritSection rotate Intervals
#' @inheritParams rotate
#' @export
#' @seealso [rotate()]
#' @examples
#' # setup example files
#' tf <- tempfile("test", fileext = ".rds")
#' saveRDS(cars, tf)
#' backup(tf)
#' backup(tf)
#'
#' backup_info(tf)
#' list_backups(tf)
#' n_backups(tf)
#' newest_backup(tf)
#' oldest_backup(tf)
#'
#' # cleanup
#' prune_backups(tf, 0)
#' n_backups(tf)
#' file.remove(tf)
#' @export
#' @return `backup_info()` returns a `data.frame` similar to the output of
#'   [file.info()]
backup_info <- function(
  file,
  dir = dirname(file)
){
  if (is_pure_BackupQueueIndex(file, dir = dir))
    BackupQueueIndex$new(file, dir = dir)$files
  else if (is_pure_BackupQueueDateTime(file, dir = dir))
    BackupQueueDateTime$new(file, dir = dir)$files
  else
    BackupQueue$new(file, dir = dir)$files
}




#' @export
#' @return `list_backups()` returns the paths to all backups of `file`
#' @rdname backup_info
list_backups <- function(
  file,
  dir = dirname(file)
){
  BackupQueue$new(file, dir = dir)$files$path
}



#' @rdname backup_info
#' @export
#' @return `n_backups()` returns the number of backups of `file` as an `integer`
#'   scalar
n_backups <- function(
  file,
  dir = dirname(file)
){
  if (!is_pure_BackupQueue(file, dir = dir)){
    warning(
      "Found index as well as timestamped backups for '", file, "'. ",
      "This is fine, but some rotor functions might not work as expected",
      "on such files.",
      call. = FALSE
    )
  }
  BackupQueue$new(file, dir = dir)$n
}




#' @return `newest_backup()` and `oldest_backup()` return the paths to the
#'   newest or oldest backup of `file` (or an empty `character` vector if none exist)
#' @export
#' @rdname backup_info
newest_backup <- function(
  file,
  dir = dirname(file)
){
  bq <- BackupQueue$new(file, dir = dir)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(file, dir = dir) ||
    is_pure_BackupQueueDateTime(file, dir = dir),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", file, "' has both:\n",
    paste("~ ", bq$files$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(file, dir = dir)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(file, dir = dir)
  }

  first(bq$files$path)
}




#' @export
#' @rdname backup_info
oldest_backup <- function(
  file,
  dir = dirname(file)
){
  bq <- BackupQueue$new(file, dir = dir)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(file, dir = dir) ||
    is_pure_BackupQueueDateTime(file, dir = dir),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", file, "' has both:\n",
    paste("~ ", bq$files$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(file, dir = dir)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(file, dir = dir)
  }

  last(bq$files$path)
}
