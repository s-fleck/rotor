#' @rdname BackupQueue
#' @export
BackupQueueDateTime <- R6::R6Class(
  "BackupQueueDateTime",
  inherit = BackupQueue,
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      max_backups = Inf,
      compression = FALSE,
      format = "%Y-%m-%d--%H-%M-%S"
    ){
      self$file <- file
      self$backup_dir <- backup_dir
      self$fmt <- format
      self$compression <- compression
      self$max_backups <- max_backups

      self
    },

    fmt = NULL,

    push_backup = function(
      compression = FALSE,
      overwrite = FALSE,
      now = Sys.time()
    ){
      assert_valid_datetime_format(self$fmt)
      assert_valid_compression(compression)

      if (is_Date(now))
        now <- as.POSIXct(as.character(now))

      stopifnot(
        is_scalar_logical(overwrite),
        is_scalar_POSIXct(now)
      )

      # generate new filename
      name <- file.path(
        self$backup_dir,
        tools::file_path_sans_ext(basename(self$file))
      )

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
        overwrite = overwrite
      )

      self
    },


    prune = function(
      max_backups
    ){
      assert(is_scalar(max_backups))

      if (!should_prune(self, max_backups))
        return(self)


      if (is_integerish(max_backups) && is.finite(max_backups)){
        # prune based on number of backups
        backups   <- rev(sort(self$backups$path))
        to_remove <- backups[(max_backups + 1):length(backups)]

      } else {
        # prune based on dates and intervals
        if (is_parsable_date(max_backups)){
          limit     <- parse_date(max_backups)
          to_remove <- self$backups$path[as.Date(as.character(self$backups$timestamp)) < limit]

        } else if (is_parsable_datetime(max_backups)){
          limit     <- parse_datetime(max_backups)
          to_remove <- self$backups$path[self$backups$timestamp < limit]

        } else if (is_parsable_interval(max_backups)){
          # interval like strings
          interval <- parse_interval(max_backups)

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

    file_remove(to_remove)
    self
  }),

  active = list(

    last_backup = function(){
      max(self$backups$timestamp)
    },

    backups = function(){
      res <- super$backups

      if (nrow(res) < 1){
        return(EMPTY_BACKUPS_DATETIME)
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
