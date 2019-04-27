#' @rdname rotate
#' @export
rotate_date <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d",
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  prerotate = identity,
  postrotate = identity,
  overwrite = FALSE,
  create_file = TRUE,
  now = Sys.Date(),
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  assert(is_scalar_logical(create_file))

  res <- backup_date(
    file = file,
    age = age,
    format = format,
    size = size,
    max_backups = max_backups,
    compression = compression,
    prerotate = prerotate,
    postrotate = postrotate,
    overwrite = overwrite,
    now = now,
    dry_run = dry_run,
    verbose = verbose
  )

  file_remove(file, dry_run = dry_run, verbose = verbose)

  if (create_file){
    file_create(file, dry_run = dry_run, verbose = verbose)
  }

  res
}




#' @rdname rotate
#' @export
backup_date <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d",
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  prerotate = identity,
  postrotate = identity,
  overwrite = FALSE,
  now = Sys.Date(),
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  stopifnot(
    is_scalar_character(file) && file.exists(file),
    is.null(age) || is_scalar(age),
    is_valid_date_format(format),
    is_scalar_integerish(size),
    is.infinite(max_backups) || is_n0(max_backups) || is.character(max_backups) || is_Date(max_backups),
    is_scalar_logical(compression),
    is.function(prerotate),
    is.function(postrotate),
    is_scalar_logical(overwrite),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose),
    is_scalar_Date(now)
  )

  bq <- BackupQueueDate$new(file, format = format)

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
    prerotate(file)
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

  if (is_parsable_date(age))
    is_backup_older_than_date(bq$last_backup, age)

  if (is_parsable_interval(age))
    is_backup_older_than_interval(bq$last_backup, age, now)
}
