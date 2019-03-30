get_name_components <- function(
  file,
  potential_children
){
  assert(is_scalar_character(file))

  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)

  # identify descent files
  children <- get_children(file, potential_children)

  if (!length(children)){
    return(character())
  }

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
  file,
  potential_children
){
  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)
  pat <- sprintf("^%s\\..*\\.%s\\.*", name, ext)
  sort(grep(pat, potential_children, value = TRUE))
}




find_children <- function(
  file
){
  assert(is_scalar_character(file))
  assert(dir.exists(dirname(file)))

  get_children(
    file,
    list.files(dirname(file), full.names = TRUE)
  )
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
