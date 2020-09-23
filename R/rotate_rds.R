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
#' @param on_change_only `logical` scalar. Rotate only if `object` is different
#'   from the object saved in `file`.
#'
#' @inheritParams base::saveRDS
#' @inheritDotParams rotate
#' @inheritParams  rotate_date
#' @inheritDotParams rotate_date
#' @inheritDotParams rotate_time
#'
#' @return the path to `file` (invisibly)
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
#' @rdname rotate_rds
#' @export
rotate_rds <- function(
  object,
  file = "",
  ascii = FALSE,
  version = NULL,
  compress = TRUE,
  refhook = NULL,
  ...,
  on_change_only = FALSE
){
  rotate_rds_internal(
    object = object,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    refhook = refhook,
    ...,
    on_change_only = on_change_only,
    fun = rotate
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
  ...,
  age = -1L,
  on_change_only = FALSE
){
  rotate_rds_internal(
    object = object,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    refhook = refhook,
    ...,
    age = age,
    on_change_only = on_change_only,
    fun = rotate_date
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
  ...,
  age = -1L,
  on_change_only = FALSE
){
  rotate_rds_internal(
    object = object,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    refhook = refhook,
    ...,
    age = age,
    on_change_only = on_change_only,
    fun = rotate_time
  )
}




rotate_rds_internal <- function(
  object,
  file,
  ascii,
  version,
  compress,
  refhook,
  ...,
  on_change_only,
  fun
){
  assert(is_scalar_character(file))
  assert(is_scalar_bool(on_change_only))

  if (file.exists(file)){
    if (on_change_only){
      comp <- readRDS(file)
      if (
        identical(object, comp) ||
        (inherits(object, "data.table") && assert_namespace("data.table") && isTRUE(all.equal(object, comp)))
      ){
        message(ObjectHasNotChangedMessage("not rotating: object has not changed"))
        return(invisible(file))
      }
    }
    fun(file, ...)
  }

  saveRDS(
    object = object,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    refhook = refhook
  )

  invisible(file)
}
