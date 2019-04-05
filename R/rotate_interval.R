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

  idx <- BackupQueueIndex$new(file)
  if (length(idx$backups)){
    warning(
      "Rotating by interval '", interval, "', but indexed backups exist:\n",
      paste("-", idx$backups, "\n"), call. = FALSE)
  } else {
    rm(idx)
  }

  bq <- BackupQueueDate$new(file)


  if (bq$has_backups){
    iv <- parse_interval(interval)

    if (identical(iv$unit, "week")){
      weeks <- dint::as_date_yw(parse_date(bq$backup_matrix[, "sfx"]))
      do_backup <- dint::Sys.date_yw() >= max(weeks) + 1 * iv$value

    } else if (identical(iv$unit, "month")){
      months    <- dint::as_date_ym(parse_date(bq$backup_matrix[, "sfx"]))
      do_backup <- dint::Sys.date_ym() >= max(months) + 1 * iv$value

    } else if (identical(iv$unit, "quarter")){
      quarters    <- dint::as_date_yq(parse_date(bq$backup_matrix[, "sfx"]))
      do_backup <- dint::Sys.date_yq() >= max(quarters) + 1 * iv$value

    } else if (identical(iv$unit, "year")){
      years     <- dint::get_year(parse_date(bq$backup_matrix[, "sfx"]))
      do_backup <- dint::get_year(Sys.Date()) >= max(years) + 1 * iv$value
    }

  } else {
    do_backup <- TRUE
  }

  if (do_backup){
    bq$push_backup()
  }


  bq$backups[[1]]
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

