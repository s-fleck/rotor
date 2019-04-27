last <- function(x){
  x[[length(x)]]
}




first <- function(x){
  x[[1]]
}



# rename a file by incrementing a timestamp in the filename. very narrow function
# only used during unit tests
replace_date_stamp <- function(
  x,
  increment = NULL,
  replace = NULL
){
  if (is.character(replace)) replace <- as.Date(replace)
  assert(is.null(increment) || is_scalar_integerish(increment))
  assert(is.null(replace)   || is_scalar_Date(replace))
  assert(is.null(increment) + is.null(replace) == 1)

  splt <- strsplit(x, ".", fixed = TRUE)

  assert(all(sapply(splt, length) == 3))

  splt <- lapply(splt, function(.x) {
    .x <- as.list(.x)
    .x[[2]] <- parse_date(.x[[2]])
    data.frame(
      name = .x[[1]],
      sfx  = .x[[2]],
      ext  = .x[[3]]
    )
  })

  splt <- do.call(rbind, splt)

  if (!is.null(increment)){
    splt$sfx <- splt$sfx + increment

  } else if (!is.null(replace)){
    splt$sfx <- replace
  }

  new_name <- apply(splt, 1, paste, collapse = ".")

  assert(all(file.exists(x)))
  assert(isTRUE(!file.exists(new_name)))
  file.rename(x, new_name)
  new_name
}




readable_size <- function(
  x
){
  readablifiy <- function(.x){
    for (unit in c("B", "kB", "MB", "GB", "TB")){
      if (max(abs(.x)) < 1024 || unit == "TB")
        break
      else
        .x <- .x / 1024
    }
    return(paste(round(.x, 1), unit))
  }

  vapply(x, readablifiy, character(1))
}




expect_snapshot_unchanged <- function(snap){
  expect_true(!any(utils::changedFiles(snap)$changes))
}
