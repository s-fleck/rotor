#' Rotate or backup files
#'
#' Functions starting with `backup` create backups of a `file`, while functions
#' starting with `rotate` do the same but also replace the original `file`
#' with an empty one (this is useful for log rotation)
#'
#' @param file `character` scalar: file to backup/rotate
#' @param age minimum age after which to backup/rotate a file; can be
#'   - a `character` scalar representing an Interval in the form
#'     `"<number> <interval>"` (e.g. `"2 months"`, see *Intervals* section below).
#'   - a `Date` or a `character` scalar [representing a Date][parse_date] for
#'     a fixed point in time after which to backup/rotate.
#'
#' @param max_backups maximum number of backups to keep
#'   - an `integer` scalar: Maximum number of backups to keep
#'
#'   In addtion for timestamped backups the following value are supported:
#'   - a `Date` scalar: Remove all backups before this date
#'   - a `character` scalar representing a Date in ISO format
#'     (e.g. `"2019-12-31"`)
#'   - a `character` scalar representing an Interval in the form
#'     `"<number> <interval>"` (see below for more info)
#'
#' @param size scalar `integer` or `character`. Backup/rotate if `file` is
#'   larger than this size. `Integers` are interpretet as
#'   bytes. You can pass `character` vectors that contain a file size suffix
#'   like `1k` (kilobytes), `3M` (megabytes), `4G` (gigabytes),
#'   `5T`` (terabytes). Instead of these short forms you can also be explicit
#'   and use the IEC suffixes `KiB`, `MiB`, `GiB`, `TiB`. In Both cases
#'   `1` kilobyte is `1024` bytes, 1 `megabyte` is `1024` kilobytes, etc... .
#'
#' @param compression Whether or not backups should be compressed
#'   - `FALSE` for uncompressed backups,
#'   - `TRUE` for zip compression; uses [zipr::zip()] if available,
#'   - a scalar `integer` between `1` and `9` to specify a compression
#'     level (requires [zip::zipr()], see its documentation for details)
#'   - `"base::zip()"` or `"zip::zipr"` to force a specific zip command
#'
#' @param dry_run `logical` scalar. If `TRUE` no changes are applied to the
#'   file system (no files are created or deleted)
#'
#' @param verbose `logical` scalar. If `TRUE` additional informative `messages`
#'   are printed
#'
#' @return `file` as a `character` scalar (invisibly)
#'
#' @section Side Effects:
#' `backup()`, `backup_date()`, and `backup_time()` creates a new file on the
#' file system (if the specified conditions are met). It may also delete
#' backups if `max_backup` is reached.
#'
#' `rotate()`, `rotate_date()` and `rotate_time()` does the same, but in
#' addtion replaces the input `file` with an empty file (or not if
#' `create_file == FALSE`)
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
#' `backup_time(myfile, max_backups = "1 year")` on `2019-03-01`, it will create
#' a backup and then remove all backups of `myfile` before `2019-01-01`.
#' @seealso [n_backups()]
#' @export
rotate <- function(
  file,
  size = 0,
  max_backups = Inf,
  compression = FALSE,
  create_file = TRUE,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  backup(
    file,
    size = size,
    max_backups = max_backups,
    compression = compression,
    dry_run = dry_run,
    verbose = verbose
  )

  file_remove(file, dry_run = dry_run, verbose = verbose)

  if (create_file)
    file_create(file, dry_run = dry_run, verbose = verbose)

  invisible(file)
}


#' @rdname rotate
#' @export
backup <- function(
  file,
  size = 0,
  max_backups = Inf,
  compression = FALSE,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  stopifnot(
    is_scalar_character(file) && file.exists(file)
  )
  size <- parse_size(size)

  bq <- BackupQueueIndex$new(
    file,
    max_backups = max_backups
  )

  if (file.size(file) > size){
    bq$push_backup(
      compression = compression,
      dry_run = dry_run,
      verbose = verbose
    )
  }

  bq$prune(
    max_backups,
    dry_run = dry_run,
    verbose = verbose
  )

  invisible(file)
}



#' @param x `character` scalar (`1k`, `1.5g`) etc
#' @return a `numeric` scalar (can be `double` or `integer`)
#' @noRd
#'
parse_size <- function(x){
  assert(is_scalar(x) && !is.na(x))

  if (is_integerish(x)){
    return(as.integer(x))
  } else {
    assert(is.character(x))
  }

  num  <- as.numeric(substr(x, 1, nchar(x) -1L))
  unit <- parse_info_unit(substr(x, nchar(x), nchar(x)))

  res <- num * unit
  assert(is_scalar(res) && !is.na(res) && is_scalar_numeric(res))
  res
}




parse_info_unit <- function(x){
  assert(is_scalar_character(x))
  x <- tolower(x)

  iec <- c("KiB", "MiB", "GiB", "TiB")

  if (x %in% iec)
    x <- substr(x, 1, 1)

  valid_units <- c("k", "m", "g", "t")

  assert(
    x %in% valid_units,
    "'", x, "' is not one of the following valid file size units: ",
    paste(c(valid_units, iec), collapse = ", ")
  )

  res <- switch(
    tolower(x),
    k = 2^10,
    m = 2^20,
    g = 2^30,
    t = 2^40,
    NULL
  )

  assert(
    !is.null(res),
    "Something went wrong when parsing the unit of information. ",
    "Please file a bug report"
  )
  res
}
