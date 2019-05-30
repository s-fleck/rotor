#' Rotate or backup files
#'
#' Functions starting with `backup` create backups of a `file`, while functions
#' starting with `rotate` do the same but also replace the original `file`
#' with an empty one (this is useful for log rotation)
#'
#' @param file `character` scalar: file to backup/rotate
#'
#' @param age minimum age after which to backup/rotate a file; can be
#'   - a `character` scalar representing an Interval in the form
#'     `"<number> <interval>"` (e.g. `"2 months"`, see *Intervals* section below).
#'   - a `Date` or a `character` scalar representing a Date for
#'     a fixed point in time after which to backup/rotate. See `format` for
#'     which Date/Datetime formats are supported by rotor.
#'
#' @param format a scalar `character` that can be a subset of of valid
#'   `strftime()` formatting strings. The default setting is
#'   `"%Y-%m-%d--%H-%M-%S"`.
#'   * You can use an arbitrary number of dashes anywhere in the format, so
#'     `"%Y-%m-%d--%H-%M-%S"` and `"%Y%m%d%H%M%S"` are both legal.
#'   * `T` and `_` can also be used as separators. For example, the following
#'     datetime formats are also possible:
#'       `%Y-%m-%d_%H-%M-%S` (Python logging default),
#'       `%Y%m%dT%H%M%S` ([ISO 8601](https://en.wikipedia.org/wiki/ISO_8601))
#'   * All datetime components except `%Y` are optional. If you leave out part
#'     of the timestamp, the first point in time in the period is assumed. For
#'     example (assuming the current year is 2019) `%Y` is identical to
#'     `2019-01-01--00-00-00`.
#'   * The timestamps must be lexically sortable, so `"%Y-%m-%d"` is legal,
#'     `"%m-%d-%Y"` and `%Y-%d` are not.
#'
#' @param now The current `Date` or time (`POSIXct`) as a scalar. You can pass a
#'   custom value here to to override the real system time. As a convenience you
#'   can also pass in `character` strings that follow the guidelines outlined
#'   above for `format`, but please note that these differ from the formats
#'   understood by [as.POSIXct()] or [as.Date()].
#'
#' @param max_backups maximum number of backups to keep
#'   - an `integer` scalar: Maximum number of backups to keep
#'
#'   In addition for timestamped backups the following value are supported:
#'   - a `Date` scalar: Remove all backups before this date
#'   - a `character` scalar representing a Date in ISO format
#'     (e.g. `"2019-12-31"`)
#'   - a `character` scalar representing an Interval in the form
#'     `"<number> <interval>"` (see below for more info)
#'
#' @param size scalar `integer`, `character` or `Inf`. Backup/rotate only if
#'   `file` is larger than this size. `Integers` are interpreted as bytes. You
#'   can pass `character` vectors that contain a file size suffix like `1k`
#'   (kilobytes), `3M` (megabytes), `4G` (gigabytes), `5T` (terabytes). Instead
#'   of these short forms you can also be explicit and use the IEC suffixes
#'   `KiB`, `MiB`, `GiB`, `TiB`. In Both cases `1` kilobyte is `1024` bytes, 1
#'   `megabyte` is `1024` kilobytes, etc... .
#'
#' @param backup_dir `character` scalar. The directory in which the backups
#'   of `file` are stored (defaults to `dirname(file)`)
#'
#' @param compression Whether or not backups should be compressed
#'   - `FALSE` for uncompressed backups,
#'   - `TRUE` for zip compression; uses [zip()]
#'   - a scalar `integer` between `1` and `9` to specify a compression
#'     level (requires the
#'     [zip](https://CRAN.R-project.org/package=zip) package,
#'     see its documentation for details)
#'   - the `character` scalars `"utils::zip()"` or `"zip::zipr"` to force a
#'     specific zip command
#'
#' @param dry_run `logical` scalar. If `TRUE` no changes are applied to the
#'   file system (no files are created or deleted)
#'
#' @param verbose `logical` scalar. If `TRUE` additional informative `messages`
#'   are printed
#'
#' @param create_file `logical` scalar. If `TRUE` create an empty file in
#'   place of `file` after rotating.
#'
#' @param overwrite `logical` scalar. If `TRUE` overwrite backups if a backup
#'   of the same name (usually due to timestamp collision) exists.
#'
#' @return `file` as a `character` scalar (invisibly)
#'
#' @section Side Effects:
#' `backup()`, `backup_date()`, and `backup_time()` may create files (if the
#' specified conditions are met). They may also delete backups, based on
#' `max_backup`.
#'
#' `rotate()`, `rotate_date()` and `rotate_time()` do the same, but in
#' addition delete the input `file`, or replace it with an empty file if
#' `create_file == TRUE` (the default).
#'
#' @section Intervals:
#'
#' In **rotor**, an interval is a character string in the form
#' `"<number> <interval>"`. The following intervals are possible:
#'  `"day(s)"`, `"week(s)"`, `"month(s)"`, `"quarter(s)"`, `"year(s)"`.
#' The plural `"s"` is optional (so `"2 weeks"` and `"2 week"` are equivalent).
#' Please be aware that weeks are
#' [ISOweeks](https://en.wikipedia.org/wiki/ISO_week_date)
#' and start on Monday (not Sunday as in some countries).
#'
#' Interval strings can be used as arguments when backing up or rotating files,
#' or for pruning backup queues (i.e. limiting the number of backups of a
#' single) file.
#'
#' When rotating/backing up `"1 months"` means "make a new backup if the last
#' backup is from the preceding month". E.g if the last backup of `myfile`
#' is from `2019-02-01` then `backup_time(myfile, age = "1 month")` will only
#' create a backup if the current date is at least `2019-03-01`.
#'
#' When pruning/limiting backup queues, `"1 year"` means "keep at least most
#' one year worth of backups". So if you call
#' `backup_time(myfile, max_backups = "1 year")` on `2019-03-01`, it will create
#' a backup and then remove all backups of `myfile` before `2019-01-01`.
#' @seealso [list_backups()]
#' @export
#'
#' @examples
#' # setup example file
#' tf <- tempfile("test", fileext = ".rds")
#' saveRDS(cars, tf)
#'
#' # create two backups of `tf``
#' backup(tf)
#' backup(tf)
#' list_backups(tf)  # find all backups of a file
#'
#' # If `size` is set, a backup is only created if the target file is at least
#' # that big. This is more useful for log rotation than for backups.
#' backup(tf, size = "100 mb")  # no backup becuase `tf` is to small
#' list_backups(tf)
#'
#' # If `dry_run` is TRUE, backup() only shows what would happen without
#' # actually creating or deleting files
#' backup(tf, size = "0.1kb", dry_run = TRUE)
#'
#' # rotate() is the same as backup(), but replaces `tf`` with an empty file
#' rotate(tf)
#' list_backups(tf)
#' file.size(tf)
#' file.size(list_backups(tf))
#'
#' # prune_backups() can remove old backups
#' prune_backups(tf, 1)  # keep only one backup
#' list_backups(tf)
#'
#' # rotate/backup_date() adds a date instead of an index
#' # you should not mix index backups and timestamp backups
#' # so we clean up first
#' prune_backups(tf, 0)
#' saveRDS(cars, tf)
#'
#' # backup_date() adds the date instead of an index to the filename
#' backup_date(tf)
#'
#' # `age` sets the minimum age of the last backup before creating a new one.
#' # the example below creates no new backup since it's less than a week
#' # since the last.
#' backup_date(tf, age = "1 week")
#'
#' # `now` overrides the current date.
#' backup_date(tf, age = "1 year", now = "2999-12-31")
#' list_backups(tf)
#'
#' # backup_time() creates backups with a full timestamp
#' backup_time(tf)
#'
#' # It's okay to mix backup_date() and backup_time()
#' list_backups(tf)
#'
#' # cleanup
#' prune_backups(tf, 0)
#' file.remove(tf)
rotate <- function(
  file,
  size = 1,
  max_backups = Inf,
  compression = FALSE,
  backup_dir = dirname(file),
  create_file = TRUE,
  dry_run = FALSE,
  verbose = dry_run
){
  rotate_internal(
    file,
    size = size,
    max_backups = max_backups,
    compression = compression,
    backup_dir = backup_dir,
    create_file = create_file,
    dry_run = dry_run,
    verbose = verbose,
    do_rotate = TRUE
  )
}




#' @rdname rotate
#' @export
backup <- function(
  file,
  size = 0,
  max_backups = Inf,
  compression = FALSE,
  backup_dir = dirname(file),
  dry_run = FALSE,
  verbose = dry_run
){
  rotate_internal(
    file,
    size = size,
    max_backups = max_backups,
    compression = compression,
    backup_dir = backup_dir,
    dry_run = dry_run,
    verbose = verbose,
    create_file = FALSE,
    do_rotate = FALSE
  )
}




rotate_internal <- function(
  file,
  size,
  max_backups,
  compression,
  create_file,
  backup_dir,
  dry_run,
  verbose,
  do_rotate
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

  bq <- BackupQueueIndex$new(
    file,
    backup_dir = backup_dir,
    max_backups = max_backups,
    compression = compression
  )

  if (bq$should_rotate(size = size, verbose = verbose)){
    bq$push_backup()
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




#' @description `prune_backups()` physically deletes all backups of a file
#'   based on `max_backups`
#' @section Side Effects:
#' `prune_backups()` may delete files, depending on `max_backups`.
#' @export
#' @rdname rotate
prune_backups <- function(
  file,
  max_backups,
  backup_dir = dirname(file),
  dry_run = FALSE,
  verbose = dry_run
){
  assert_pure_BackupQueue(file, backup_dir = backup_dir)
  assert(is_scalar_character(file))

  if (dry_run){
    DRY_RUN$activate()
    on.exit(DRY_RUN$deactivate())
  }

  bq <- BackupQueueIndex$new(file, backup_dir = backup_dir)

  if (!bq$has_backups)
    bq <- BackupQueueDateTime$new(file, backup_dir = backup_dir)

  bq$prune(max_backups = max_backups)
  invisible(file)
}

