rotate_date <- function(file){

}




backup_date <- function(
  file,
  max_backups = 6,
  format = "%Y-%m-%d",
  compression = "none",
  time = Sys.time()
){
  stopifnot(
    is_scalar_character(file),
    file.exists(file),
    is_scalar_integerish(max_backups),
    is_scalar_character(compression)
  )

  # generate new filename
  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)
  sfx <- format(time, format)
  if (is_blank(ext)) {
    name_new <- paste(name, sfx, sep = ".")
  } else {
    name_new <- paste(name, sfx, ext, sep = ".")
  }
  file.copy(file, name_new, overwrite = FALSE)

  # compress
  if (compression != "none"){
    name_new <- compress_and_remove(name_new, compression = compression)
  }

  # prune backups
  backups <- find_backups(file)
  if (length(backups) > max_backups){
    prune_backups(file, max_backups, backups = sort(backups, decreasing = TRUE))
  }

  name_new
}




#' Prune list of files down to `max_backups`.
#' You must ensure that `backups` is sorted as as you want it already!
#'
#' @param file
#' @param max_backups
#' @param backups
#'
#' @return `character` vector of the names of the remaining backups
#' @noRd
prune_backups <- function(
  file,
  max_backups,
  backups = find_backups(file)
){
  to_remove <- backups[(max_backups + 1):length(backups)]
  file.remove(to_remove)
  backups[1:max_backups]
}
