



parse_date <- function(x){
  if (is_Date(x)){
    return(x)

  } else if (is_POSIXct(x)){
    return(as.Date(x))

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
  gsub("T|-|_", "", as.character(x))
}




standardize_date_stamp <- function(x){
  gsub("-|_", "", as.character(x))
}
