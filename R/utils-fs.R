file_rename <- function(
  from,
  to,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  msg_file_rename(from, to, dry_run = dry_run, verbose = verbose)
  if (dry_run) return()

  file.rename(from, to)
}




#' @param ... passed to file.copy
#' @noRd
file_copy <- function(
  from,
  to,
  ...,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  msg_file_copy(from, to, dry_run = dry_run, verbose = verbose)
  if (dry_run) return()

  file.copy(from, to, ...)
}




file_create <- function(
  ...,
  showWarnings = TRUE,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  msg_file_create(..., dry_run = dry_run, verbose = verbose)
  if (dry_run) return()

  file.create(..., showWarnings = showWarnings)
}





file_remove<- function(
  ...,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  msg_file_remove(..., dry_run = dry_run, verbose = verbose)
  if (dry_run) return()

  file.remove(...)
}





msg_file_copy <- function(from, to, dry_run, verbose){
  stopifnot(
    is.character(from),
    is.character(to),
    is_scalar_logical(verbose),
    is_scalar_logical(dry_run)
  )

  if (!verbose) return()

  to <- ifelse(
    dirname(from) == dirname(to),
    basename(to),
    to
  )

  message(paste0("[dry_run] "[dry_run], "copying:"))
  message(paste0("[dry_run] "[dry_run], "+ ", from , " -> ", to, "\n"))
}




msg_file_remove <- function(..., dry_run, verbose){
  files <- c(...)
  stopifnot(
    is.character(files),
    is_scalar_logical(verbose),
    is_scalar_logical(dry_run)
  )

  if (!verbose) return()

  message(paste0("[dry_run] "[dry_run], "removing:"))
  message(paste0("[dry_run] "[dry_run], "- ", files, "\n"))
}



msg_file_rename <- function(from, to, dry_run, verbose){
  stopifnot(
    is.character(from),
    is.character(to),
    is_scalar_logical(verbose),
    is_scalar_logical(dry_run)
  )

  if (!verbose)
    return()

  sel <- from != to
  from <- from[sel]
  to <- to[sel]

  if (!length(from))
    return()

  to <- ifelse(
    dirname(from) == dirname(to),
    basename(to),
    to
  )

  message(paste0("[dry_run] "[dry_run], "renaming:"))
  message(paste0("[dry_run] "[dry_run], "~ ", from , " -> ", to, "\n"))
}



msg_file_create <- function(..., dry_run, verbose){
  files <- c(...)
  stopifnot(
    is.character(files),
    is_scalar_logical(verbose),
    is_scalar_logical(dry_run)
  )

  if (!verbose) return()

  message(paste0("[dry_run] "[dry_run], "creating:"))
  message(paste0("[dry_run] "[dry_run], "+ ", files, "\n"))
}
