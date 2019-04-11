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
#' @param prerotate,postrotate a `function` with a single argument (a file path
#'   as `character` scalar). `preorate()` and `postrotate()` are
#'   called before/after the backup is rotated.
#' @param dry_run `logical` scalar. If `TRUE` no changes are applied to the
#'   file system (no files are created or deleted)
#' @param verbose `logical` scalar. If `TRUE` additional informative `messages`
#'   are printed
#'
#' @return
#'  If a creating a backup is triggered and `postrotate` is `NULL`, the path to
#'  the newly created file is  returned as a `character` scalar. If `postrotate`
#'  is a function, whatever `postrotate()` returns is returned.
#'
#'  If no backup is created, an empty `character()` vector is returned.
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
  prerotate = identity,
  postrotate = identity,
  overwrite = FALSE,
  dry_run = FALSE,
  verbose = dry_run
){
  stopifnot(
    is_scalar_character(file) && file.exists(file),
    is.null(age) || is_scalar(age),
    is_valid_date_format(format),
    is_scalar_integerish(min_size),
    is.infinite(n_backups) || is_n0(n_backups) || is.character(n_backups) || is_Date(n_backups),
    is_scalar_logical(compression),
    is.function(prerotate),
    is.function(postrotate),
    is_scalar_logical(overwrite),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose)
  )

  bq <- BackupQueueDate$new(file)

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

  now <- Sys.Date()

  if (is_backup_time_necessary(bq, age, now)){
    prerotate(bq$file)
    bq$push_backup(
      now = now,
      format = format,
      compression = compression,
      dry_run = dry_run,
      verbose = verbose
    )
    res <- postrotate(bq$backups$path[[1]])
  } else {
    res <- character()
  }

  bq$prune(n_backups, dry_run = dry_run, verbose = verbose)
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
