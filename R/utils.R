last <- function(x){
  x[[length(x)]]
}




first <- function(x){
  x[[1]]
}



# rename a file by incrementing a timestamp in the filename. very narrow function
# only used during unit tests
increment_date_stamp <- function(x, increment){
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
  splt$sfx <- splt$sfx + increment
  new_name <- apply(splt, 1, paste, collapse = ".")

  assert(all(file.exists(x)))
  assert(isTRUE(!file.exists(new_name)))
  file.rename(x, new_name)
  new_name
}



