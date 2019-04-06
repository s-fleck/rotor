#' @param file `character` scalar: The base file.
#' @param potential_backups `chracter` vector: list of files that could
#'   potentially be backups for `file` (and follow the rotor naming convention)
get_backups <- function(
  file,
  potential_backups,
  sfx_patterns
){
  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)
  sfx_patterns <- paste0("(", sfx_patterns, ")", collapse = "|")

  if (is_blank(ext)){
    pat <- sprintf("^%s\\.%s\\.*$", name, sfx_patterns)

  } else {
    pat <- sprintf("^%s\\.%s\\.%s\\.*$", name, sfx_patterns, ext)
  }

  sort(grep(pat, potential_backups, value = TRUE))
}




filenames_as_matrix <- function(
  file,
  backups
){
  if (length(backups) < 1){
    return(NULL)
  }

  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)
  name_end <- attr(gregexpr(name, backups[[1]])[[1]], "match.length") + 1L
  a <- strsplit_at_pos(backups, name_end)

  if (!is_blank(ext)){
    ext_start <- unlist(gregexpr(ext, a[, 2]))
    b <- strsplit_at_pos(a[, 2], ext_start - 1L)
    res <- cbind(a[, 1], b)
    colnames(res) <- c("name", "sfx", "ext")
  } else {
    res <- cbind(a, "")
    colnames(res) <- c("name", "sfx", "ext")
  }

  assert(is.matrix(res))
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
