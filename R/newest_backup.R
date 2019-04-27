#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
newest_backup <- function(
  x
){
  bq <- BackupQueue$new(x)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(x) || is_pure_BackupQueueDateTime(x),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", x, "' has both:\n",
    paste("~ ", bq$backups$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(x)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(x)
  }

  first(bq$backups$path)
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
oldest_backup <- function(
  x
){
  bq <- BackupQueue$new(x)
  if (!bq$has_backups){
    return(character())
  }

  assert(
    is_pure_BackupQueueIndex(x) || is_pure_BackupQueueDateTime(x),
    "Can only determine newest backup for files that only have either indexed ",
    "or timestamped backups, but '", x, "' has both:\n",
    paste("~ ", bq$backups$path, collapse = "\n")
  )

  bq <- BackupQueueDateTime$new(x)

  if (!bq$has_backups){
    bq <- BackupQueueIndex$new(x)
  }

  last(bq$backups$path)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
n_backups <- function(x){
  BackupQueue$new(x)$n_backups
}



is_pure_BackupQueueIndex <- function(
  x
){
  identical(BackupQueueDateTime$new(x)$n_backups, 0L)
}




is_pure_BackupQueueDateTime <- function(
  x
){
  identical(BackupQueueIndex$new(x)$n_backups, 0L)
}
