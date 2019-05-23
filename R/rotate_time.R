#' @rdname rotate
#' @export
rotate_time <- function(
  file,
  age  = -1,
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  format = "%Y-%m-%d--%H-%M-%S",
  backup_dir = dirname(file),
  overwrite = FALSE,
  create_file = TRUE,
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
  age  = -1,
  size =  1,
  max_backups = Inf,
  compression = FALSE,
  format = "%Y-%m-%d--%H-%M-%S",
  backup_dir = dirname(file),
  overwrite = FALSE,
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
    is_scalar_bool(do_rotate),
    is_scalar_bool(dry_run),
    is_scalar_bool(verbose),
    is_scalar_bool(create_file)
  )

  assert_pure_BackupQueue(file, backup_dir = backup_dir, warn_only = TRUE)

  if (dry_run){
    DRY_RUN$activate()
    on.exit(DRY_RUN$deactivate())
  }

  bq <- BackupQueueDateTime$new(file, fmt = format, backup_dir = backup_dir)

  if (bq$should_rotate(size = size, age = age, now = now)){
    bq$push_backup(
      now = now,
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
    return(is_backup_older_than_datetime(bq$last_rotation, age))

  if (is_parsable_rotation_interval(age))
    return(is_backup_older_than_interval(bq$last_rotation, age, now))
}
