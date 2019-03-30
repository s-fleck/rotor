list_backups = function(file){
  backups <- list.files(
    dirname(file),
    pattern = paste0(basename(file), "\\..*(\\.zip){0,1}$"),
    full.names = TRUE
  )
}




do_rollover = function(file, compress, last_rollover, compress_args){
  new_ts <- format(Sys.time(), format = timestamp_fmt)
  assert(file.exists(file))
  backups <- private$list_backups()
  outname <- paste0(file, ".", new_ts)

  if (any(grepl(paste0(basename(outname), "(\\.zip){0,1}"), backups))){
    stop(sprintf("Log rollover aborted: A backup for '%s' already exists. ", outname))
  }

  if (compress) {
    outname <- paste0(outname, ".zip")
    do.call(
      utils::zip,
      c(list(zipfile = outname, files  = file), compress_args)
    )
    file.remove(file)

  } else {
    file.rename(file, outname)
  }

  last_rollover <- Sys.time()
  invisible(self)
}






do_rollover = function(file, compress, compress_args){
  assert(file.exists(file))
  backups <- private$list_backups()
  idx_pad <- 1L

  if (length(backups) > 0){
    idx <- names(backups)
    ext <- ifelse(tools::file_ext(backups) == "zip", ".zip", "")
    sorder <- order(sort(as.integer(idx)), decreasing = TRUE)
    idx <- idx[sorder]
    ext <- ext[sorder]

    assert(!anyNA(idx))
    idx_pad <- max(nchar(as.character(as.integer(idx) + 1L)))

    # walk instead of vectorized file.rename to prevent accidental overwrites
    walk(
      seq_along(idx),
      function(i){
        file.rename(
          paste0(file, ".", idx[[i]], ext[[i]]),
          paste0(file, ".", as.integer(idx[[i]]) + 1L, ext[[i]])
        )
      }
    )
  }

  if (compress) {
    utils::capture.output(do.call(
      utils::zip,
      c(list(zipfile = paste0(file, ".1.zip"), files  = file), compress_args)
    ))
    file.remove(file)

  } else {
    file.rename(file, paste0(file, ".1"))
  }

  backups <- private$list_backups()
  file.rename(backups, autopad_backup_index(backups))
  invisible(self)
}








get_backup_index <- function(
  x
){
  vapply(
    strsplit(x, ".", fixed = TRUE),
    function(.x) {
      .r <- .x[[length(.x)]]
      if (identical(.r, "zip"))  .r <- .x[[length(.x) - 1L]]
      assert(!is.na(as.integer(.r)))
      .r
    },
    character(1)
  )
}




get_backup_timestamp <- function(
  x
){
  vapply(
    strsplit(x, ".", fixed = TRUE),
    function(.x) {
      .r <- .x[[length(.x)]]
      if (identical(.r, "zip"))  .r <- .x[[length(.x) - 1L]]
      .r
    },
    character(1)
  )
}




autopad_backup_index <- function(
  x
){
  assert(is.character(x) && length(x) > 0)
  bn  <- gsub("\\.\\d*(zip){0,1}", "", x)
  idx <- get_backup_index(x)
  int <- as.integer(idx)
  ext <- ifelse(tools::file_ext(x) == "zip", ".zip", "")
  new_idx <- pad_left(int, max(nchar(as.character(int))), "0")
  paste0(bn, ".", new_idx, ext)
}
