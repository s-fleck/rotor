rotate <- function(file){

}




#' Title
#'
#' @param file
#' @param max_backups
#' @param compression
#'
#' @return
#' @export
#'
#' @examples
backup <- function(
  file,
  max_backups = Inf,
  compression = "none"
){
  stopifnot(
    is_scalar_character(file),
    file.exists(file),
    is.infinite(max_backups) || is_scalar_integerish(max_backups),
    is_scalar_character(compression)
  )

  backups <- find_backups(file)
  sfx <- "1"


  # rename and prune backups, pad suffix if necessary
  if (length(backups)){
    backups_new <- get_name_components(file, backups)
    backups_new[, "sfx"] <- as.integer(backups_new[, "sfx"]) + 1L
    backups_new[, "sfx"] <- pad_left(backups_new[, "sfx"], pad = "0")
    sfx  <- pad_left(sfx, width = max(nchar(backups_new[, "sfx"])), "0")

    backups_new <- apply(backups_new, 1, paste, collapse = ".")
    file.rename(rev(backups), rev(backups_new))

    if (length(backups_new) >= max_backups){
      backups_new <- prune_tail(file, max_backups, backups = backups_new)
      pad_backup_index(file, backups = backups_new)
    }
  }

  # generate new filename
  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)
  if (is_blank(ext)) {
    name_new <- paste(name, sfx, sep = ".")
  } else {
    name_new <- paste(name, sfx, ext, sep = ".")
  }


  file.copy(file, name_new, overwrite = FALSE)

  if (compression != "none"){
    name_new <- compress_and_remove(name_new, compression = compression)
  }

  invisible(name_new)
}




pad_backup_index <- function(
  file,
  backups = find_backups(file)
){
  backups <- find_backups(file)
  backups_new <- get_name_components(file, backups)
  backups_new[, "sfx"] <- as.integer(backups_new[, "sfx"])
  backups_new[, "sfx"] <- pad_left(backups_new[, "sfx"], pad = "0")
  backups_new <- apply(backups_new, 1, paste, collapse = ".")

  file.rename(rev(backups), rev(backups_new))
  backups_new
}
