msg_prune_backups <- function(file, to_remove, dry_run, verbose){
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




msg_file_copy <- function(old, new, dry_run, verbose){
  assert(is_scalar_character(old))
  assert(is.character(new))
  assert(is_scalar_logical(verbose))
  assert(is_scalar_logical(dry_run))

  if (!verbose) return()

  message("[dry_run] "[dry_run], "'", old, "' -> ", basename(new))
}




msg_file_remove <- function(file, dry_run, verbose){
  assert(is_scalar_character(file))
  assert(is_scalar_logical(verbose))
  assert(is_scalar_logical(dry_run))

  if (!verbose) return()

  message("[dry_run] "[dry_run], "deleting '", file, "'")
}
