#' Helpers for working with backups
#'
#' `n_backups()`, `list_backups()`, `newest_backup()` and `oldest_backup()`
#' display information on the backups (that follow the naming conventions of
#' rotor) of a single file.
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
#' list_backups(tf)
#' n_backups(tf)
#' newest_backup(tf)
#' oldest_backup(tf)
#'
#' # cleanup
#' prune_backups(tf, 0)
#' n_backups(tf)
#' file.remove(tf)
list_backups <- function(
  file,
  backup_dir = dirname(file)
){
  BackupQueue$new(file, backup_dir = backup_dir)$backups$path
}




#' @rdname list_backups
#' @export
#' @return `list_backups()` returns the paths to all backups of `file`
n_backups <- function(
  file,
  backup_dir = dirname(file)
){
  if (is_impure_BackupQueue(file, backup_dir = backup_dir)){
    warning(
      "Found index as well as timestamped backups for '", file, "'. ",
      "This is fine, but some rotor functions might not work as expected",
      "on such files.",
      call. = FALSE
    )
  }
  BackupQueue$new(file, backup_dir = backup_dir)$n_backups
}




#' @return `newest_backup()` and `oldest_backup()` return the paths to the
#'   newest or oldest backup of `file` (or an empty `character` vector if none exist)
#' @export
#' @rdname list_backups
newest_backup <- function(
  file,
  backup_dir = dirname(file)
){
  bq <- BackupQueue$new(file, backup_dir = backup_dir)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(file, backup_dir = backup_dir) ||
    is_pure_BackupQueueDateTime(file, backup_dir = backup_dir),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", file, "' has both:\n",
    paste("~ ", bq$backups$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(file, backup_dir = backup_dir)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(file, backup_dir = backup_dir)
  }

  first(bq$backups$path)
}




#' @export
#' @rdname list_backups
oldest_backup <- function(
  file,
  backup_dir = dirname(file)
){
  bq <- BackupQueue$new(file, backup_dir = backup_dir)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(file, backup_dir = backup_dir) ||
    is_pure_BackupQueueDateTime(file, backup_dir = backup_dir),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", file, "' has both:\n",
    paste("~ ", bq$backups$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(file, backup_dir = backup_dir)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(file, backup_dir = backup_dir)
  }

  last(bq$backups$path)
}




is_pure_BackupQueueIndex <- function(
  file,
  backup_dir = dirname(file)
){
  identical(BackupQueueDateTime$new(file, backup_dir = backup_dir)$n_backups, 0L)
}




is_pure_BackupQueueDateTime <- function(
  file,
  backup_dir = dirname(file)
){
  identical(BackupQueueIndex$new(file, backup_dir = backup_dir)$n_backups, 0L)
}




is_impure_BackupQueue <- function(
  file,
  backup_dir = dirname(file)
){
  BackupQueueDateTime$new(file, backup_dir = backup_dir)$n_backups > 0 &&
  BackupQueueIndex$new(file, backup_dir = backup_dir)$n_backups > 0
}




assert_pure_BackupQueue <- function(
  file,
  backup_dir = dirname(file)
){
  if (is_impure_BackupQueue(file, backup_dir = backup_dir)){
    stop(
      "Operation not possible because indexed as well as timestamped backups
       exist for '", file, "'. ",
      call. = FALSE
    )
  }
}
