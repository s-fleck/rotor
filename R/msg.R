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





should_prune <- function(
  obj,
  max_backups,
  dry_run,
  verbose
){
  assert(is_scalar_logical(dry_run))
  assert(is_scalar_logical(verbose))
  do_msg <- {if (verbose) message else function(...) NULL}

  if (!obj$has_backups){
    do_msg(
      "[dry_run] "[dry_run],
      "Nothing to prune; no backups found for '", obj$file, "'"
    )
    return(FALSE)
  }

  if (is.infinite(max_backups) || is.na(max_backups)){
    do_msg(
      "[dry_run] "[dry_run],
      "Nothing to prune; `max_backups` is set to '", format(max_backups), "'"
    )
    return(FALSE)
  }

  TRUE
}
