#' Title
#'
#' @param file
#' @param n_backups
#' @param format `character` scalar. Only the following date formats are
#'   supported by rotor: `"%Y-%m-%d"`, `"%Y%m%d"`, `"%Y-%m"`, `"%Y%m"`, `"%Y"`
#' @param compression
#' @param date scalar `Date`. Today's Date. Can be overriden for
#'   testing / debugging.
#'
#' @return
#' @export
#'
#' @examples
backup_date <- function(
  file,
  n_backups = Inf,
  format = "%Y-%m-%d",
  compression = "none",
  date = Sys.Date()
){
  stopifnot(
    is_scalar_character(file),
    file.exists(file),
    is.infinite(n_backups) || is_scalar_integerish(n_backups),
    is_scalar_character(compression),
    assert(is_valid_date_format(format))
  )

  # generate new filename
  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)
  sfx <- format(date, format)
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
  if (length(backups) > n_backups){
    prune_backups_head(file, n_backups, backups = backups)
  }

  invisible(name_new)
}




is_valid_date_format <- function(x){
  is_scalar_character(x) &&
  x %in% c("%Y-%m-%d", "%Y%m%d", "%Y-%m", "%Y%m", "%Y")
}




is_parsable_date <- function(x){
  tryCatch(
    {parse_date(x); TRUE},
    error = function(...) FALSE
  )
}




parse_date <- function(x){

  prep_string <- function(.x){
    if (identical(nchar(.x), 4L))
      .x <- paste0(.x, "-01-01")

    else if (identical(nchar(.x), 6L))
      .x <- paste(substr(.x, 1 , 4), substr(.x, 5, 6), "01", sep = "-")

    else if (identical(nchar(.x), 7L))
      .x <- paste0(.x, "-01")

    else if (identical(nchar(.x), 8L))
      .x <- paste(substr(.x, 1 ,4), substr(.x, 5, 6), substr(.x, 7, 8), sep = "-")

    else if (identical(nchar(.x), 10L))
      return(.x)

    else
      stop("Cannot parse Date from'", x, "'")
  }

  res <- as.Date(vapply(x, prep_string, character(1)))
  assert(!anyNA(res))
  res
}
