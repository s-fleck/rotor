#' Title
#'
#' @param file file to back up/rotate
#' @param n_backups understands scalars of different types
#'   - an `integer` scalar: Maximum number of backups to keep
#'   - a `Date` scalar: Remove all backups before this date
#'   - a `character` scalar representing a Date in ISO format
#'     (e.g. `"2019-12-31"`)
#'   - a `character` scalar representing an Interval in the form
#'     `"<number> <interval>"` (e.g. `"2 weeks"`, `"1 year"`).
#'     Possible intervals are
#'     `"day(s)"`, `"week(s)"`, `"month(s)"`, `"quarter(s)"`, `"year(s)"`.
#' @param compression
#' @param postrotate
#' @param postrotate_args
#' @param prerotate
#' @param prerotate_args
#' @param dry_run
#' @param verbose
#' @param interval
#' @param min_size
#' @param create_file
#'
#' @return
#' @export
#'
#' @examples
backup_time <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d",
  min_size = 1,
  n_backups = Inf,
  compression = FALSE,
  create_file = FALSE,
  prerotate = NULL,
  postrotate = NULL,
  overwrite = FALSE,
  dry_run = FALSE,
  verbose = dry_run
){
  stopifnot(
    is_scalar_character(file) && file.exists(file),
    is.null(age) || is_scalar(age),
    is_scalar_integerish(min_size),
    is.infinite(n_backups) || is_n(n_backups),
    is_scalar_logical(compression),
    is_scalar_logical(create_file),
    is.null(prerotate)  || is.function(prerotate),
    is.null(postrotate) || is.function(postrotate),
    is_scalar_logical(overwrite),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose)
  )

  bq <- BackupQueue$new(file)
  bd <- BackupQueueDate$new(file)

  if (bq$has_backups){
    # Warn if indexed backups exist
    bi <- BackupQueueIndex$new(file)
    idx_backups <- paste(setdiff(bi$backups$path, bd$backups$path))
    if (length(idx_backups)){
      warning(
        "Backing up by timestamp, but indexed backups exist already:\n",
        paste("-", setdiff(bi$backups$path, bd$backups$path), collapse = "\n"),
        call. = FALSE
      )
    }
  }
  rm(bq)

  bd <- BackupQueueDate$new(file)
  if (bd$has_backups){
    # now <- Sys.time()
    # do_backup <-
    #   check_backup_time(age, now)  ||
    #   check_backup_date(age, now)

  } else {
    do_backup <- TRUE
  }

  do_backup <- TRUE

  if (do_backup){
    bq$push_backup()
  }


  bq$prune(n_backups)$backups$path[[1]]
}




check_backup_date <- function(x, now){
  if (!is_parsable_date(x)){
    return(FALSE)
  }

  now > parse_date(x)
}



check_backup_time <- function(x, now){
  if (!is_parsable_interval(x)){
    return(FALSE)
  }

  iv <- parse_interval(x)

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
}




is_parsable_interval <- function(x){
  tryCatch(
    {parse_interval(x); TRUE},
    error = function(e) FALSE
  )

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

