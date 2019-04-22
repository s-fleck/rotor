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
  n_backups,
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

  if (is.infinite(n_backups) || is.na(n_backups)){
    do_msg(
      "[dry_run] "[dry_run],
      "Nothing to prune; `n_backups` is set to '", format(n_backups), "'"
    )
    return(FALSE)
  }

  TRUE
}
