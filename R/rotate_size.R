#' Title
#'
#' @param file
#' @param size scalar `integer` or `character`. Integers are interpretet as
#'   bytes. You can pass `character` vectors that contain a file size suffix
#'   like `1k` (kilobytes), `3M` (megabytes), `4G` (gigabytes),
#'   `5T`` (terabytes). Please note that those use the binary definitions,
#'   so 1 kilobyte is 1024 bytes, 1 megabyte is 1024 kilobytes, etc.. .
#' @param age
#' @param max_backups
#' @param compression
#' @param postrotate
#' @param postrotate_args
#' @param prerotate
#' @param prerotate_args
#' @param dry_run
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
rotate_size <- function(
  file,
  size,
  age,
  max_backups = Inf,
  compression = FALSE,
  prerotate = NULL,
  prerotate_args = NULL,
  postrotate = NULL,
  postrotate_args = NULL,
  dry_run = FALSE,
  verbose = FALSE
){
  stopifnot(
    is_scalar_character(file) && file.exists(file)
  )
  size <- parse_size(size)

  if (file.size(file) < size){
    if (verbose) {
      message(sprintf(
        "Not rotating '%s': Filesize (%s) is less than the limit (%s)",
        basename(file),
        fmt_bytes(file.size(file)),
        fmt_bytes(size)
      ))
    }
    return(character())
  } else {
    res <- backup(file, max_backups = max_backups, compression = compression)
    if (verbose) message(sprintf("Rotated '%s' to '%s'", file, res))
  }

  unlink(file)
  file.create(file)

  res
}



fmt_bytes <- function(x){
  format(structure(x, class = "object_size"), "auto")
}


parse_size <- function(x){
  assert(is_scalar(x) && !is.na(x))


  if (is_integerish(x)){
    return(as.integer(x))
  } else {
    assert(is.character(x))
  }

  num  <- as.integer(substr(x, nchar(x) -1L, nchar(x) -1L))
  unit <- parse_info_unit(substr(x, nchar(x), nchar(x)))

  res <- as.integer(num * unit)
  assert(is_scalar(res) && !is.na(res) && is.integer(res))
  res
}




parse_info_unit <- function(x){
  assert(is_scalar_character(x))
  valid_units <- c("k", "m", "g", "t")

  assert(
    x %in% valid_units,
    "'", x, "' is not one of the following valid file size units: ",
    paste(valid_units, collapse = ", ")
  )

  res <- switch(
    tolower(x),
    k = 2^10,
    m = 2^20,
    g = 2^30,
    t = 2^40,
    NULL
  )

  assert(
    !is.null(res),
    "Something went wrong when parsing the unit of information. ",
    "Please file a bug report"
  )
  res
}
