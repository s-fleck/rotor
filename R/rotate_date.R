#' @rdname rotate
#' @export
rotate_date <- function(
  file,
  age  = 1,
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  format = "%Y-%m-%d",
  backup_dir = dirname(file),
  overwrite = FALSE,
  create_file = TRUE,
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
  age  = 1,
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  format = "%Y-%m-%d",
  backup_dir = dirname(file),
  overwrite = FALSE,
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

  bq <- BackupQueueDate$new(
    file,
    fmt = format,
    backup_dir = backup_dir,
    compression = compression
  )

  # backup
  if (bq$should_rotate(size = size, age = age, now = now)){
    bq$push_backup(
      now = now,
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
