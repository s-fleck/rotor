
is_pure_BackupQueueIndex <- function(
  file,
  backup_dir = dirname(file)
){
  identical(BackupQueueDateTime$new(file, backup_dir = backup_dir)$n_backups, 0L)
}




is_pure_BackupQueueDateTime <- function(
  file,
  backup_dir = dirname(file)
){
  bi <- BackupQueueIndex$new(file, backup_dir = backup_dir)
  identical(bi$n_backups, 0L) || min(bi$backups$index) > 1L
}




is_pure_BackupQueue <- function(
  file,
  backup_dir = dirname(file)
){
  bi <- BackupQueueIndex$new(file, backup_dir = backup_dir)

  if (bi$n_backups < 1){
    TRUE
  } else if (identical(min(bi$backups$index), 1L)){
    # check if min index is 1 to filter out BackupQueueIndex that are truely
    # BackupQueueDate but only have integer like timestamps
    identical(
      try(BackupQueueDateTime$new(file, backup_dir = backup_dir)$n_backups, silent = TRUE),
      0L
    )
  } else {
    TRUE
  }
}




assert_pure_BackupQueue <- function(
  file,
  backup_dir = dirname(file),
  warn_only = FALSE
){
  if (is_pure_BackupQueue(file, backup_dir = backup_dir))
    return(TRUE)

  msg <- paste0(
    "Indexed as well as timestamped backups exist for '", file, "'.\n",
    paste("*", list_backups(file), collapse = "\n")
  )

  if (warn_only){
    warning(msg, call. = FALSE)
    FALSE
  } else {
    stop("Operation not possible: ", msg,  call. = FALSE)
  }
}




is_parsable_rotation_interval <- function(x){
  is_scalar(x) && (
    is_integerish(x) ||
    is.infinite(x) ||
    grepl("\\d+\\syear|quarter|month|week|day", x)
  )
}




is_valid_date_format <- function(x){
  is_scalar_character(x) &&
    x %in% c(
      "%Y-%m-%d",
      "%Y-%m",
      "%Y%m%d",
      "%Y%m",
      "%Y"
    )
}




assert_valid_date_format <- function(x){
  xdep <- deparse(substitute(x))
  if (!is_valid_datetime_format(x)){
    stop(
      "`", xdep, "` is not a valid date format but ", preview_object(x),
      ". See ?rotate for details."
    )
  }
  TRUE
}




is_valid_date_format <- function(
  x
){
  if (!is_scalar_character(x))
    return(FALSE)

  x <- standardize_datetime_stamp(x)

  if (nchar(x) > 6)
    return(FALSE)

  is_valid_datetime_format(x)
}




assert_valid_datetime_format <- function(x){
  xdep <- deparse(substitute(x))
  if (!is_valid_datetime_format(x))
    stop(
      "`", xdep, "` is not a valid datetime format but ", preview_object(x),
      ". See ?rotate for details."
    )
  else
    TRUE
}




is_valid_datetime_format <- function(
  x
){
  if (!is_scalar_character(x))
    return(FALSE)

  standardize_datetime_stamp(x) %in% c(
    "%Y%m%d%H%M%S",
    "%Y%m%d%H%M",
    "%Y%m%d%H",
    "%Y%m%d",
    "%Y%m",
    "%Y"
  )
}




is_parsable_datetime <- function(x){
  is_scalar(x) && (
    is_Date(x) ||
    is_POSIXct(x) ||
    grepl("^\\d{4,14}$", standardize_datetime_stamp(x))
  )
}





is_parsable_date <- function(x){
  is_scalar(x) && (
    is_Date(x) ||
    is_scalar(x) && grepl("^\\d{4,8}$", standardize_datetime_stamp(x))
  )
}




is_backup_older_than_datetime <- function(
  backup_date,
  datetime
){
  if (is_Date(backup_date))
    backup_date <- as.POSIXct(as.character(backup_date))

  assert(is_scalar_POSIXct(backup_date))
  assert(is_parsable_datetime(datetime))
  backup_date < parse_datetime(datetime)
}




is_backup_older_than_interval <- function(
  backup_date,
  interval,
  now
){
  if (is_POSIXct(backup_date))
    backup_date <- as.Date(as.character(backup_date))

  if (is_POSIXct(now)){
    now <- as.Date(as.character(now))
  } else if (is.character(now)){
    now <- as.Date(parse_datetime(now))
  }

  assert(is_scalar_Date(backup_date))
  assert(is_scalar_Date(now))
  iv <- parse_rotation_interval(interval)


  as_period <- switch(
    iv$unit,
    day     = identity,
    week    = dint::as_date_yw,
    month   = dint::as_date_ym,
    quarter = dint::as_date_yq,
    year    = dint::get_year
  )

  as_period(backup_date) + 1L * iv$value <= as_period(now)
}




assert_valid_compression <- function(compression){
  assert(
    is_scalar_atomic(compression) && (
      compression %in% c("utils::zip", "zip::zipr") ||
        compression %in% 1:9 ||
        is_bool(compression)
    ),
    '`compression` must be `TRUE`, `FALSE`, or an integer between 1 and 9',
    'or the character scalers "utils::zip" or "zip::zipr" not: ',
    preview_object(compression)
  )
}




is_zipcmd_available <- function(cmd = Sys.getenv("R_ZIPCMD", "zip")){

  if (is_blank(cmd)){
    return(FALSE)
  }

  if (.Platform$OS.type == "windows"){
    suppressWarnings(res <- system2("where", cmd, stderr = NULL, stdout = NULL))
  } else {
    res <- tryCatch(
       system2("command", paste("-v", cmd), stderr = NULL, stdout = NULL),
       warning = function(w) {99}
    )
  }

  assert(is_scalar(res))
  res == 0
}




is_dir <- function(
  x
){
  file.exists(x) && file.info(x)$isdir
}
