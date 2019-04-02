#' Title
#'
#' @param file
#' @param size scalar `integer` or `character`. Integers are interpretet as
#'   bytes. You can pass `character` vectors that contain a file size suffix
#'   like `1k` (kilobytes), `3M` (megabytes), `4G` (gigabytes),
#'   `5T`` (terabytes). Please note that those use the binary definitions,
#'   so 1 kilobyte is 1024 bytes, 1 megabyte is 1024 kilobytes, etc.. .
#' @param age
#' @param n_backups
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
rotate_interval <- function(
  file,
  interval,
  n_backups = Inf,
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
    res <- backup(file, n_backups = n_backups, compression = compression)
    if (verbose) message(sprintf("Rotated '%s' to '%s'", file, res))
  }

  unlink(file)
  file.create(file)

  res
}





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

