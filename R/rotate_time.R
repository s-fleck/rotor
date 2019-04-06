#' Title
#'
#'
#' @section Intervals:
#'
#' In **rotor**, an interval is a character string in the form
#' `"<number> <interval>"`. The following intervals are possible:
#'  `"day(s)"`, `"week(s)"`, `"month(s)"`, `"quarter(s)"`, `"year(s)"`.
#' The plural `"s"` is optional (so `"2 weeks"` and `"2 week"` are equivalent).
#' Please be aware that weeks are
#' [ISOweeks][https://en.wikipedia.org/wiki/ISO_week_date]
#' and start on Monday (not Sunday as in some countries).
#'
#' Interval strings can be used as arguments when backing up or rotating files,
#' or for pruning backup queues (i.e. limiting the number of backups of a
#' single) file.
#'
#' When rotating/backing up `"1 months"` means "make a new backup if the last
#' backup is from the preceeding month". E.g if the last backup of `myfile`
#' is from `2019-02-01` then `backup_time(myfile, age = "1 month")` will only
#' create a backup if the current date is at least `2019-03-01`.
#'
#' When pruning/limiting backup queues, `"1 year"` means "keep at least most
#' one year worth of backups". So if you call
#' `backup_time(myfile, n_backups = "1 year")` on `2019-03-01`, it will create
#' a backup and then remove all backups of `myfile` before `2019-01-01`.
#'
#'
#' @param file file to back up/rotate
#' @param age
#'   - a `character` scalar representing an Interval in the form
#'     `"<number> <interval>"` (see section Intervals).
#'     Backup/rotate if the last backup is older than
#'     that (e.g. `"2 months"`). See examples
#'
#'
#'   - a `Date` or a `character` scalar [representing a Date][parse_date].
#'     Backup/rotate if the last backup was before that date
#' @param n_backups understands scalars of different types
#'   - an `integer` scalar: Maximum number of backups to keep
#'   - a `Date` scalar: Remove all backups before this date
#'   - a `character` scalar representing a Date in ISO format
#'     (e.g. `"2019-12-31"`)
#'   - a `character` scalar representing an Interval in the form
#'     `"<number> <interval>"`
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
#'
#'
#'
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
    is.infinite(n_backups) || is_n(n_backups) || is.character(n_backups) || is_Date(n_backups),
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
  now <- Sys.Date()

  do_backup <-
    is.null(age) ||
    !bd$has_backups ||
    check_backup_interval(age, bd$last_backup, now) ||
    check_backup_date(age, bd$last_backup)


  if (do_backup){
    bq$push_backup(now = now)
  }


  bq$prune(n_backups)$backups$path[[1]]
}




check_backup_date <- function(
  x,
  last_backup
){
  assert(is_scalar_Date(last_backup))
  if (!is_parsable_date(x)){
    return(FALSE)
  }
  last_backup < parse_date(x)
}




#' Title
#'
#' @param x an `interval`
#' @param last_backup
#' @param now
#'
#' @return
#' @export
#'
#' @examples
check_backup_interval <- function(
  x,
  last_backup,
  now
){
  assert(is_scalar_Date(last_backup))
  assert(is_scalar_Date(now))
  if (!is_parsable_interval(x)){
    return(FALSE)
  }

  iv <- parse_interval(x)

  as_period <- switch(
    iv$unit,
    week    = dint::as_date_yw,
    month   = dint::as_date_ym,
    quarter = dint::as_date_yq,
    year    = dint::get_year
  )

  as_period(last_backup) + 1L * iv$value <= as_period(now)
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




is_valid_date_format <- function(x){
  is_scalar_character(x) &&
    x %in% c("%Y-%m-%d", "%Y%m%d", "%Y-%m", "%Y%m", "%Y")
}




is_parsable_date <- function(x){
  tryCatch(
    {parse_date(x); TRUE},
    error = function(...) FALSE
  )
}




parse_date <- function(x){

  if (is_Date(x))
    return(x)

  prep_string <- function(.x){
    if (identical(nchar(.x), 4L))
      .x <- paste0(.x, "-01-01")

    else if (identical(nchar(.x), 6L))
      .x <- paste(substr(.x, 1 , 4), substr(.x, 5, 6), "01", sep = "-")

    else if (identical(nchar(.x), 7L))
      .x <- paste0(.x, "-01")

    else if (identical(nchar(.x), 8L))
      .x <- paste(substr(.x, 1 ,4), substr(.x, 5, 6), substr(.x, 7, 8), sep = "-")

    else if (identical(nchar(.x), 10L))
      return(.x)

    else
      stop("Cannot parse Date from'", x, "'")
  }

  res <- as.Date(vapply(x, prep_string, character(1)))
  assert(!anyNA(res))
  res
}
