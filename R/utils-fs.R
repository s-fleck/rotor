file_rename <- function(
  from,
  to,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  msg_file_rename(from, to, dry_run = dry_run, verbose = verbose)
  if (dry_run) {
    return(dm$move(from, to))
  }

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
  if (dry_run){
    return(dm$create(to))
  }

  file.copy(from, to, ...)
}




file_create <- function(
  ...,
  showWarnings = TRUE,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  msg_file_create(..., dry_run = dry_run, verbose = verbose)
  if (dry_run) {
    return(dm$create(...))
  }

  file.create(..., showWarnings = showWarnings)
}




file_remove<- function(
  ...,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  msg_file_remove(..., dry_run = dry_run, verbose = verbose)
  if (dry_run) {
    return(dm$remove(...))
  }

  file.remove(...)
}




file_exists<- function(
  ...,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  if (dry_run) {
    return(dm$exists(...))
  }

  file.exists(...)
}




list_files <- function(
  path = ".",
  full.names = FALSE,
  ...,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  if (dry_run) {
    res <- dm$list(path, ...)
    if (!full.names) res <- basename(res)
    return(res)
  }

  list.files(path = path, full.names = full.names, ...)
}




msg_file_copy <- function(
  from,
  to,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose  = dry_run
){
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




msg_file_remove <- function(
  ...,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose  = dry_run
){
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




msg_file_rename <- function(
  from,
  to,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose  = dry_run
){
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




msg_file_create <- function(
  ...,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose  = dry_run)
{
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




msg_prune_backups <- function(
  file,
  to_remove,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose  = dry_run
){
  assert(is_scalar_character(file))
  assert(is.character(to_remove))
  assert(is_scalar_logical(verbose))
  assert(is_scalar_logical(dry_run))

  if (!verbose) return()

  message(
    "[dry_run] "[dry_run], "pruning backups for '", file, "':\n",
    paste0("[dry_run] "[dry_run], "- ", to_remove, collapse = "\n")
  )
}




should_prune <- function(
  obj,
  max_backups
){
  if (!obj$has_backups){
    return(FALSE)
  }

  if (is.infinite(max_backups) || is.na(max_backups)){
    return(FALSE)
  }

  TRUE
}
