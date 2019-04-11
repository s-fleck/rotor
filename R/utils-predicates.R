


is_parsable_interval <- function(x){
  tryCatch(
    {parse_interval(x); TRUE},
    error = function(e) FALSE
  )
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





is_backup_older_than_date <- function(
  backup_date,
  date
){
  assert(is_scalar_Date(backup_date))
  assert(is_parsable_date(date))
  last_backup < parse_date(date)
}

#' Title
#'
#' @param x an `interval`
#' @param last_backup
#' @param now
#'
#' @return
#' @export
#'
#' @examples
is_backup_older_than_interval <- function(
  backup_date,
  interval,
  now
){
  assert(is_scalar_Date(backup_date))
  assert(is_scalar_Date(now))
  assert(is_parsable_interval(interval))

  iv <- parse_interval(interval)

  as_period <- switch(
    iv$unit,
    week    = dint::as_date_yw,
    month   = dint::as_date_ym,
    quarter = dint::as_date_yq,
    year    = dint::get_year
  )

  as_period(backup_date) + 1L * iv$value <= as_period(now)
}
