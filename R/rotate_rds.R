#' Serialize R objects to disk (with backup)
#'
#' The `rotate_rds*()` functions are wrappers around [base::saveRDS()][base::readRDS()] that
#' create a backup of the destination file (if it exists) instead of just
#' overwriting it.
#'
#' @note The default value for `age` is different for `rotate_rds_date()` (`-1`)
#'   than for [rotate_date()] (`1`) to make it a bit safer. This means if you
#'   execute `rotate_date()` twice on the same file on a given day it will
#'   silently not rotate the file, while `rotate_rds_date()` will throw an
#'   error.
#'
#' @inheritParams base::saveRDS
#' @inheritDotParams rotate
#' @inheritParams  rotate_date
#' @inheritDotParams rotate_date
#' @inheritDotParams rotate_time
#'
#' @return `NULL` (invisibly)
#' @export
#'
#' @examples
#' dest <- tempfile()
#' rotate_rds(iris, dest)
#' rotate_rds(iris, dest)
#' rotate_rds(iris, dest)
#'
#' list_backups(dest)
#'
#' # cleanup
#' unlink(list_backups(dest))
#' unlink(dest)
rotate_rds <- function(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL,
  ...
){
  if (file.exists(file))
    rotate(file, ...)

  saveRDS(
    object = object,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    refhook = refhook
  )
}




#' @rdname rotate_rds
#' @export
rotate_rds_time <- function(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL,
  age = -1L,
  ...
){
  if (file.exists(file))
    rotate_time(file, ..., age = age)

  saveRDS(
    object = object,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    refhook = refhook
  )
}




#' @rdname rotate_rds
#' @export
rotate_rds_date <- function(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL,
  age = -1L,
  ...
){
  if (file.exists(file))
    rotate_date(file, ..., age = age)

  saveRDS(
    object = object,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    refhook = refhook
  )
}
