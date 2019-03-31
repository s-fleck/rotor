#' Title
#'
#' @param file
#' @param max_backups
#' @param format
#' @param compression
#' @param time
#'
#' @return
#' @export
#'
#' @examples
backup_date <- function(
  file,
  max_backups = Inf,
  format = "%Y-%m-%d",
  compression = "none",
  time = Sys.time()
){
  stopifnot(
    is_scalar_character(file),
    file.exists(file),
    is.infinite(max_backups) || is_scalar_integerish(max_backups),
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
    prune_head(file, max_backups, backups = backups)
  }

  invisible(name_new)
}
