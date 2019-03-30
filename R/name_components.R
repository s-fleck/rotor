get_name_components <- function(
  x,
  src
){
  assert(is_scalar_character(src))

  name <- tools::file_path_sans_ext(basename(src))
  ext  <- tools::file_ext(src)

  # identify descent files
  children <- get_children(x, src)

  # identify name parts
  name_end <- attr(gregexpr(name, children[[1]])[[1]], "match.length") + 1L
  a <- strsplit_at_pos(children, name_end)
  ext_start <- gregexpr(ext, a[, 2][[1]])[[1]]
  b <- strsplit_at_pos(a[, 2], ext_start - 1L)

  res <- cbind(a[, 1], b)
  colnames(res) <- c("name", "sfx", "ext")
  res
}




get_children <- function(
  x,
  src
){
  name <- tools::file_path_sans_ext(basename(src))
  ext  <- tools::file_ext(src)
  pat <- sprintf("^%s\\..*\\.%s\\.*", name, ext)
  grep(pat, x, value = TRUE)
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
