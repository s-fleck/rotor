#' @param format a scalar `character` that can be a subset of of valid
#'   `strftime()` formatting strings. The basic pattern is
#'   `"%Y-%m-%d--%H-%M-%S"`. The sepparators `-` and `T` are optional.
#'   * You can use an arbitrary number of dashes anywhere in the format, so
#'     `"%Y-%m-%d--%H-%M-%S"` and `"%Y%m%d%H%M%S"` are both legal.
#'   * All datetime components except `%Y` are optional. If you leave out part
#'     of the timestamp, the first point in time in the period is assumed. For
#'     example (assuming the current year is 2019) `%Y` is identical to
#'     `2019-01-01T00-00-00`.
#'   * The timestamps must be lexically sortable, so `"%Y-%m-%d"` is legal,
#'     `"%m-%d-%Y"` and `%Y-%d` are not.
#'
#' @rdname rotate
#' @export
rotate_time <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d--%H-%M-%S",
  min_size = 1,
  max_backups = Inf,
  compression = FALSE,
  prerotate = identity,
  postrotate = identity,
  overwrite = FALSE,
  create_file = TRUE,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  assert(is_scalar_logical(create_file))

  res <- backup_time(
    file = file,
    age = age,
    format = format,
    min_size = min_size,
    max_backups = max_backups,
    compression = compression,
    prerotate = prerotate,
    postrotate = postrotate,
    overwrite = overwrite,
    dry_run = dry_run,
    verbose = verbose
  )

  file_remove(file, dry_run = dry_run, verbose = verbose)

  if (create_file)
    file_create(file, dry_run = dry_run, verbose = verbose)

  res
}




#' @rdname rotate
#' @export
backup_time <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d--%H-%M-%S",
  min_size = 1,
  max_backups = Inf,
  compression = FALSE,
  now = Sys.time(),
  prerotate = identity,
  postrotate = identity,
  overwrite = FALSE,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  stopifnot(
    is_scalar_character(file) && file.exists(file),
    is.null(age) || is_scalar(age),
    is_valid_datetime_format(format),
    is_scalar_integerish(min_size),
    is.infinite(max_backups) || is_n0(max_backups) || is.character(max_backups) || is_Date(max_backups),
    is_scalar_logical(compression),
    is.function(prerotate),
    is.function(postrotate),
    is_scalar_logical(overwrite),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose)
  )

  bq <- BackupQueueDateTime$new(file, format = format)

  # Warn if indexed backups exist
  if (BackupQueue$new(file)$has_backups){
    bi <- BackupQueueIndex$new(file)
    idx_backups <- paste(setdiff(bi$backups$path, bq$backups$path))
    if (length(idx_backups)){warning(
      "Backing up by timestamp, but indexed backups exist already:\n",
      paste("-", setdiff(bi$backups$path, bq$backups$path), collapse = "\n"),
      call. = FALSE
    )}
  }

  if (is_backup_time_necessary(bq, age, now)){
    prerotate(bq$file)
    bq$push_backup(
      now = now,
      compression = compression,
      dry_run = dry_run,
      verbose = verbose
    )
    res <- postrotate(bq$backups$path[[1]])
  } else {
    res <- character()
  }

  bq$prune(max_backups, dry_run = dry_run, verbose = verbose)
  res
}




is_backup_time_necessary <- function(
  bq, age, now
){
  if (is.null(age) || !bq$has_backups)
    return(TRUE)

  if (is_parsable_datetime(age))
    return(is_backup_older_than_datetime(bq$last_backup, age))

  if (is_parsable_interval(age))
    return(is_backup_older_than_interval(bq$last_backup, age, now))
}
