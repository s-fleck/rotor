BackupQueueDateTime <- R6::R6Class(
  "BackupQueueDateTime",
  inherit = BackupQueue,
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      format = "%Y-%m-%d--%H-%M-%S"
    ){
      self$file <- file
      self$backup_dir <- backup_dir
      self$fmt <- format
      self
    },

    fmt = NULL,

    push_backup = function(
      compression = FALSE,
      overwrite = FALSE,
      now = Sys.time(),
      dry_run = getOption("rotor.dry_run", FALSE),
      verbose = getOption("rotor.dry_run", dry_run)
    ){
      assert_valid_datetime_format(self$fmt)

      if (is_Date(now))
        now <- as.POSIXct(as.character(now))

      stopifnot(
        is_scalar_logical(compression),
        is_scalar_logical(overwrite),
        is_scalar_POSIXct(now),
        is_scalar_logical(dry_run),
        is_scalar_logical(verbose)
      )

      # generate new filename
      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)
      sfx  <- format(now, format = self$fmt)

      if (is_blank(ext)) {
        name_new <- paste(name, sfx, sep = ".")
      } else {
        name_new <- paste(name, sfx, ext, sep = ".")
      }

      copy_or_compress(
        self$file,
        outname = name_new,
        compression = compression,
        add_ext = TRUE,
        overwrite = overwrite,
        dry_run = dry_run,
        verbose = verbose
      )

      self
    },


    prune = function(
      n_backups,  # minimum date/interval is the minimum date/interval to keep
      dry_run  = getOption("rotor.dry_run", FALSE),
      verbose = getOption("rotor.verbose", dry_run)
    ){
      assert(is_scalar(n_backups))

      if (!should_prune(self, n_backups, dry_run, verbose))
        return(self)


      if (is_integerish(n_backups) && is.finite(n_backups)){
        # prune based on number of backups
        backups   <- rev(sort(self$backups$path))
        to_remove <- backups[(n_backups + 1):length(backups)]

      } else {
        # prune based on dates and intervals
        if (is_parsable_date(n_backups)){
          limit     <- parse_date(n_backups)
          to_remove <- self$backups$path[as.Date(as.character(self$backups$timestamp)) < limit]

        } else if (is_parsable_datetime(n_backups)){
          limit     <- parse_datetime(n_backups)
          to_remove <- self$backups$path[self$backups$timestamp < limit]

        } else if (is_parsable_interval(n_backups)){
          # interval like strings
          interval <- parse_interval(n_backups)

          last_backup <- as.Date(as.character(self$last_backup))

          if (identical(interval[["unit"]], "year")){
            limit <- dint::first_of_year(dint::get_year(last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "quarter")){
              limit <- dint::first_of_quarter(dint::as_date_yq(last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "month")) {
              limit <- dint::first_of_month(dint::as_date_ym(last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "week")){
              limit <- dint::first_of_isoweek(dint::as_date_yw(last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "day")){
              limit <- as.Date(last_backup) - interval$value + 1L
          }

          to_remove <- self$backups$path[as.Date(as.character(self$backups$timestamp)) < limit]
       }
    }

    file_remove(to_remove, dry_run = dry_run, verbose = verbose)
    self
  }),

  active = list(

    last_backup = function(){
      max(self$backups$timestamp)
    },

    backups = function(){
      res <- super$backups

      if (nrow(res) < 1){
        return(data.frame())
      }

      sel <- vapply(res$sfx, is_parsable_datetime, logical(1))
      res <- res[sel, ]

      res$timestamp <- parse_datetime(res$sfx)
      res[order(res$timestamp, decreasing = TRUE), ]
    }
  )
)




parse_interval <- function(x){
  assert(is_scalar(x) && !is.na(x))

  if (is_integerish(x)){
    return(
      list(value = as.integer(x), unit = "day")
    )
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


  list(value = as.integer(value), unit = unit)
}




standardize_datetime_stamp <- function(x){
  gsub("T|-", "", as.character(x))
}




standardize_date_stamp <- function(x){
  gsub("-", "", as.character(x))
}




parse_interval <- function(x){
  assert(is_scalar(x) && !is.na(x))

  if (is_integerish(x)){
    return(
      list(value = as.integer(x), unit = "day")
    )
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

  list(value = as.integer(value), unit = unit)
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
