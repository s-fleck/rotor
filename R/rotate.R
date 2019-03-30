rotate <- function(file){

}


backup <- function(
  file,
  max_backups = 6,
  compression = "none"
){
  assert(is_scalar_character(file))
  assert(file.exists(file))
  assert(is_scalar_integerish(max_backups))
  assert(is_scalar_character(compression))

  children <- find_children(file)
  sfx <- "1"

  if (length(children)){
    children_new <- get_name_components(file, children)
    children_new[, "sfx"] <- as.integer(children_new[, "sfx"]) + 1L
    children_new[, "sfx"] <- pad_left(children_new[, "sfx"], pad = "0")
    sfx  <- pad_left(sfx, width = max(nchar(children_new[, "sfx"])), "0")

    children_new <- apply(children_new, 1, paste, collapse = ".")
    file.rename(rev(children), rev(children_new))

    if (length(children_new) >= max_backups){
      prune_backups(file, max_backups, children = children_new, verbose = verbose)
    }
  }

  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)
  name_new <- paste(name, sfx, ext, sep = ".")
  file.copy(file, name_new, overwrite = FALSE)

  if (compression != "none"){
    name_new <- compress_and_remove(name_new, compression = compression)
  }

  name_new
}



prune_backups = function(
  file,
  max_backups,
  children = find_children(file),
  verbose = FALSE
){
  to_remove <- sort(children)[(max_backups + 1):length(children)]
  file.remove(to_remove)

  children <- find_children(file)
  children_new <- get_name_components(file, children)
  children_new[, "sfx"] <- as.integer(children_new[, "sfx"])
  children_new[, "sfx"] <- pad_left(children_new[, "sfx"], pad = "0")
  children_new <- apply(children_new, 1, paste, collapse = ".")

  file.rename(rev(children), rev(children_new))

  children_new
}





