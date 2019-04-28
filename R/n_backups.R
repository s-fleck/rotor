#' Manage backups of a file
#'
#' `n_backups()`, `list_backups()`, `newest_backup()` and `oldest_backup()`
#' display information on the backups (that follow the naming conventions of
#' rotor) of a single file.
#'
#' @param file `character` scalar: Path to a file.
#' @inheritSection rotate Intervals
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
#' prune_backups(tf, 0)
#' n_backups(tf)
#'
#' # cleanup
#' file.remove(tf)
n_backups <- function(file){
  if (is_impure_BackupQueue(file)){
    warning(
      "Found index as well as timestamped backups for '", file, "'. ",
      "This is fine, but some rotor functions might not work as expected",
      "on such files.",
      call. = FALSE
    )
  }
  BackupQueue$new(file)$n_backups
}



#' @rdname n_backups
#' @export
#' @return `list_backups()` returns the paths to all backups of `file`
list_backups <- function(file){
  BackupQueue$new(file)$backups$path
}




#' @description `prune_backups()` phyiscally deletes all backups of a file
#'   based on `max_backups`
#' @inheritParams rotate
#' @return `prune_backups()` returns the path to the input file `file` (invisibly)
#' @export
#' @rdname n_backups
prune_backups <- function(
  file,
  max_backups
){
  assert_pure_BackupQueue(file)
  assert(is_scalar_character(file))

  bq <- BackupQueueIndex$new(file)

  if (!bq$has_backups)
    bq <- BackupQueueDateTime$new(file)

  bq$prune(max_backups = max_backups)
  invisible(file)
}


#' @return `newest_backup()` and `oldest_backup()` return the paths to the
#'   newst or oldest backup of `file` (or an empty `character` vector if none exist)
#' @export
#' @rdname n_backups
newest_backup <- function(
  file
){
  bq <- BackupQueue$new(file)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(file) || is_pure_BackupQueueDateTime(file),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", file, "' has both:\n",
    paste("~ ", bq$backups$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(file)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(file)
  }

  first(bq$backups$path)
}




#' @export
#' @rdname n_backups
oldest_backup <- function(
  file
){
  bq <- BackupQueue$new(file)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(file) || is_pure_BackupQueueDateTime(file),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", file, "' has both:\n",
    paste("~ ", bq$backups$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(file)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(file)
  }

  last(bq$backups$path)
}




is_pure_BackupQueueIndex <- function(
  file
){
  identical(BackupQueueDateTime$new(file)$n_backups, 0L)
}




is_pure_BackupQueueDateTime <- function(
  file
){
  identical(BackupQueueIndex$new(file)$n_backups, 0L)
}




is_impure_BackupQueue <- function(
  file
){
  BackupQueueDateTime$new(file)$n_backups > 0 &&
  BackupQueueIndex$new(file)$n_backups > 0
}



assert_pure_BackupQueue <- function(
  file
){
  if (is_impure_BackupQueue(file)){
    stop(
      "Operation not possible because indexed as well as timestamped backups
       exist for '", file, "'. ",
      call. = FALSE
    )
  }
}
