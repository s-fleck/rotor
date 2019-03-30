#' Get components of backup filenames
#'
#' @param file `character` scalar: The base file.
#' @param potential_backups `chracter` vector: list of files that could
#'   potentially be backups for `file` (and follow the rotor naming convention)
#'
#' @return
#'   a matrix with 2 or 3 columns: `name`, `sfx` and `ext` (optional). The
#'   number of rows depends on how many elements of `potential_backups` are
#'   really backups of `file`.
#' @noRd
#'
get_name_components <- function(
  file,
  potential_backups
){
  stopifnot(
    is_scalar_character(file),
    is.character(potential_backups)
  )

  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)

  # identify descent files
  backups <- get_backups(file, potential_backups)

  if (!length(backups)){
    return(character())
  }

  # identify name parts
  name_end <- attr(gregexpr(name, backups[[1]])[[1]], "match.length") + 1L
  a <- strsplit_at_pos(backups, name_end)

  if (!is_blank(ext)){
    ext_start <- gregexpr(ext, a[, 2][[1]])[[1]]
    b <- strsplit_at_pos(a[, 2], ext_start - 1L)
    res <- cbind(a[, 1], b)
    colnames(res) <- c("name", "sfx", "ext")
  } else {
    res <- a
    colnames(res) <- c("name", "sfx")
  }

  res
}





#' Splits a string at `pos` (removing the character at pos)
#'
#' @param x a `character` vector
#' @param pos an `integer` vector
#'
#' @noRd
strsplit_at_pos <- function(
  x,
  pos
){
  assert(all(substr(x, pos, pos) == "."))
  matrix(data = c(substr(x, 1, pos - 1L), substr(x, pos + 1L, nchar(x))), ncol = 2)
}
