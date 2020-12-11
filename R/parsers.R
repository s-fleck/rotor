#' @param x `character` scalar (`1k`, `1.5g`) etc
#' @return a `numeric` scalar (can be `double` or `integer`)
#' @noRd
#'
parse_size <- function(x){
  if (is.infinite(x)) return(x)
  assert(is_scalar(x) && !is.na(x))

  if (is_integerish(x)){
    res <- as.integer(x)
  } else if (is.numeric(x)){
    res <- as.integer(floor(x))
    warning("`x` ist not an integer file size, rounding down to ", res, " bits")

  } else if (is.character(x)){
    unit_start <- regexec("[kmgt]", tolower(x))[[1]]
    num  <- trimws(substr(x, 1, unit_start - 1L))
    unit <- trimws(substr(x, unit_start, nchar(x)))
    res  <- as.numeric(num) * parse_info_unit(unit)

  } else {
    stop(ValueError(paste("`x` is not a valid file size but ", preview_object(x))))
  }

  assert(is_scalar(res) && !is.na(res) && is_scalar_numeric(res))
  res
}




#' @param x a `character` scalar, see the `size` argument of `rotate()`
#' @return an `integer` scalar (bytes)
#' @noRd
parse_info_unit <- function(x){
  assert(is_scalar_character(x))
  x <- tolower(x)

  iec <- c("kib", "mib", "gib", "tib", "kb", "mb", "gb", "tb")

  if (x %in% iec)
    x <- substr(x, 1, 1)

  valid_units <- c("k", "m", "g", "t")

  assert(
    x %in% valid_units,
    "'", x, "' is not one of the following valid file size units: ",
    paste(c(valid_units, iec), collapse = ", ")
  )

  res <- switch(
    tolower(x),
    k = 2^10,
    m = 2^20,
    g = 2^30,
    t = 2^40,
    NULL
  )

  assert(
    !is.null(res),
    "Something went wrong when parsing the unit of information. ",
    "Please file a bug report"
  )
  res
}




# datetime ----------------------------------------------------------------

parse_date <- function(x){
  if (is_Date(x)){
    return(x)

  } else if (is_POSIXct(x)){
    return(as.Date(as.character(x)))

  } else if (!is.character(x) && !is_integerish(x)) {
    stop(
      "`", deparse(substitute(x)), "` must be a character or Date, ",
      "not ", preview_object(x), call. = FALSE
    )
  }

  x <- standardize_date_stamp(x)
  x <- prep_ymd(x)
  res <- as.Date(x)

  assert(!anyNA(res))
  res
}




parse_datetime <- function(x){
  if (is_POSIXct(x)){
    return(x)
  } else if (is_Date(x)) {
    return(as.POSIXct(as.character(x)))
  } else if (!is.character(x) && !is_integerish(x)) {
    stop(
      "`", deparse(substitute(x)), "` must be a character, Date, or POSIXt, ",
      "not ", preview_object(x), call. = FALSE
    )
  }

  x <- standardize_datetime_stamp(x)

  dd <- strsplit_at_pos(x, 8)
  dd[, 1] <- prep_ymd(dd[, 1])
  dd[, 2] <- prep_hms(dd[, 2])

  res <- as.POSIXct(paste(dd[, 1], dd[, 2]))
  assert(!anyNA(res))
  res
}




prep_ymd <- function(.x){
  assert(all(nchar(.x) %in% c(8, 6, 4)))
  y <- substr(.x, 1, 4)
  m <- ifelse(nchar(.x) > 4, substr(.x, 5, 6), "01")
  d <- ifelse(nchar(.x) > 6, substr(.x, 7, 8), "01")
  paste(y, m, d, sep = "-")
}




prep_hms <- function(.x){
  assert(all(nchar(.x) %in% c(0, 2, 4, 6)))
  h <- ifelse(!is_blank(.x) , substr(.x, 1, 2), "00")
  m <- ifelse(nchar(.x) >  2, substr(.x, 3, 4), "00")
  s <- ifelse(nchar(.x) >  4, substr(.x, 5, 6), "00")
  paste(h, m, s, sep = ":")
}




standardize_datetime_stamp <- function(x){
  gsub("T|-|_|\\s", "", as.character(x))
}




standardize_date_stamp <- function(x){
  gsub("-|_|\\s", "", as.character(x))
}





# rotation interval -------------------------------------------------------


parse_rotation_interval <- function(x){
  if (is_rotation_interval(x))
    return(x)

  assert(is_scalar(x) && !is.na(x))

  if (is.infinite(x))
    return(rotation_interval(value = Inf, unit = "day"))

  if (is_integerish(x)){
    return(rotation_interval(value = as.integer(x), unit = "day"))
  } else {
    assert(is.character(x))
  }

  splt <- strsplit(x, "\\s")[[1]]
  assert(identical(length(splt), 2L))

  value <- splt[[1]]
  unit  <- splt[[2]]

  valid_units <- c("day", "week", "month", "quarter", "year")
  unit <- gsub("s$", "", tolower(trimws(unit)))

  assert(unit %in% valid_units)
  value <- as.integer(value)
  assert(!is.na(value))

  rotation_interval(value = value, unit = unit)
}




rotation_interval <- function(value, unit){
  structure(list(value = value, unit = unit), class = "rotation_interval")
}




is_rotation_interval <- function(
  x
){
  inherits(x, "rotation_interval")
}
