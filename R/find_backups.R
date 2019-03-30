#' Find backups of `file`
#'
#' @inheritParams get_name_components
#' @export
find_backups <- function(
  file
){
  stopifnot(
    is_scalar_character(file),
    dir.exists(dirname(file))
  )

  get_backups(
    file,
    list.files(dirname(file), full.names = dirname(file) != ".")
  )
}




#' Identify which files are backups of `file`
#'
#' @inheritParams get_name_components
#' @noRd
get_backups <- function(
  file,
  potential_backups
){
  stopifnot(
    is_scalar_character(file),
    is.character(potential_backups)
  )

  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)

  if (is_blank(ext)){
    pat = paste0(name, "\\.[^.]+$")

  } else {
    pat <- sprintf("^%s\\..*\\.%s\\.*", name, ext)
  }

  sort(grep(pat, potential_backups, value = TRUE))
}
