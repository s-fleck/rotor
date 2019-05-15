
#' @rdname rotate
#' @export
rotate_date <- function(
  file,
  age = NULL,
  format = "%Y-%m-%d",
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  overwrite = FALSE,
  create_file = TRUE,
  backup_dir = dirname(file),
  now = Sys.Date(),
  dry_run = FALSE,
  verbose = dry_run
){
  rotate_date_internal(
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
    create_file = create_file,
    do_rotate = TRUE
  )
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
  overwrite = FALSE,
  backup_dir = dirname(file),
  now = Sys.Date(),
  dry_run = FALSE,
  verbose = dry_run
){
  rotate_date_internal(
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




rotate_date_internal <- function(
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
    is_scalar_bool(overwrite),
    is_scalar_bool(dry_run),
    is_scalar_bool(verbose),
    is_scalar_bool(create_file),
    is_scalar_bool(do_rotate),
    is_scalar(now)
  )
  assert_valid_date_format(format)
  assert(!is_dir(file))

  now <- parse_date(now)

  if (dry_run){
    DRY_RUN$activate()
    on.exit(DRY_RUN$deactivate())
  }

  size <- parse_size(size)

  bq <- BackupQueueDate$new(file, format = format, backup_dir = backup_dir)

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


  # backup
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


  # prune
  bq$prune(max_backups)


  # rotate
  if (do_rotate){
    file_remove(file)

    if (create_file){
      file_create(file)
    }
  }


  invisible(file)
}
