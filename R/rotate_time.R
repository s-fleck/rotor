#' @rdname rotate
#' @export
rotate_time <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d--%H-%M-%S",
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  overwrite = FALSE,
  create_file = TRUE,
  backup_dir = dirname(file),
  now = Sys.time(),
  dry_run = FALSE,
  verbose = dry_run
){
  rotate_time_internal(
    file = file,
    age = age,
    format = format,
    size = size,
    max_backups = max_backups,
    compression = compression,
    overwrite = overwrite,
    backup_dir = backup_dir,
    now = now,
    dry_run = dry_run,
    verbose = verbose,
    do_rotate = TRUE,
    create_file = create_file
  )
}




#' @rdname rotate
#' @export
backup_time <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d--%H-%M-%S",
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  overwrite = FALSE,
  backup_dir = dirname(file),
  now = Sys.time(),
  dry_run = FALSE,
  verbose = dry_run
){
  rotate_time_internal(
    file = file,
    age = age,
    format = format,
    size = size,
    max_backups = max_backups,
    compression = compression,
    overwrite = overwrite,
    backup_dir = backup_dir,
    now = now,
    dry_run = dry_run,
    verbose = verbose,
    do_rotate = FALSE,
    create_file = FALSE
  )
}




rotate_time_internal <- function(
  file,
  age,
  format,
  size,
  max_backups,
  compression,
  overwrite,
  create_file,
  backup_dir,
  now,
  do_rotate,
  dry_run,
  verbose
){
  stopifnot(
    is_scalar_character(file) && file_exists(file),
    is.null(age) || is_scalar(age),
    is_scalar(size),
    is.infinite(max_backups) || is_n0(max_backups) || is.character(max_backups) || is_Date(max_backups),
    is_scalar_logical(overwrite),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose),
    is_scalar_logical(create_file),
    is_scalar_bool(do_rotate)
  )
  assert_valid_date_format(format)
  assert(!is_dir(file))

  now  <- parse_datetime(now)
  size <- parse_size(size)

  if (dry_run){
    DRY_RUN$activate()
    on.exit(DRY_RUN$deactivate())
  }


  bq <- BackupQueueDateTime$new(file, format = format, backup_dir = backup_dir)

  # Warn if indexed backups exist
  if (BackupQueue$new(file, backup_dir = backup_dir)$has_backups){
    bi <- BackupQueueIndex$new(file, backup_dir = backup_dir)
    idx_backups <- paste(setdiff(bi$backups$path, bq$backups$path))
    if (length(idx_backups)){warning(
      "Backing up by timestamp, but indexed backups exist already:\n",
      paste("-", setdiff(bi$backups$path, bq$backups$path), collapse = "\n"),
      call. = FALSE
    )}
  }

  if (
    file.size(file) > size &&
    is_backup_time_necessary(bq, age, now)
  ){
    bq$push_backup(
      now = now,
      compression = compression,
      overwrite = overwrite
    )
  } else {
    do_rotate <- FALSE
  }

  bq$prune(max_backups)


  if (do_rotate){
    file_remove(file)

    if (create_file)
      file_create(file)
  }


  invisible(file)
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
