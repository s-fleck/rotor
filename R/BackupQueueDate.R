BackupQueueDate <- R6::R6Class(
  "BackupQueue",
  inherit = BackupQueue,
  public = list(
    push_backup = function(
      format = "%Y-%m-%d",
      compression = FALSE,
      overwrite = FALSE
    ){
      # generate new filename
      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)
      sfx <- format(Sys.Date(), format = format)

      if (is_blank(ext)) {
        name_new <- paste(name, sfx, sep = ".")
      } else {
        name_new <- paste(name, sfx, ext, sep = ".")
      }

      if (file.exists(name_new) && !overwrite){
        stop("Backup exists and `overwrite == FALSE`")
      }

      file.copy(self$file, name_new, overwrite = FALSE)
      name_new <- compress_and_remove(name_new, compression = compression)
      self
    },

    prune = function(
      n_backups
    ){
      assert(is_scalar(n_backups))

      if (is.infinite(n_backups) || is.na(n_backups))
        return(self)

      if (is.character(n_backups) && is_parsable_date(n_backups))
        n_backups <- parse_date(n_backups)

      if (is_integerish(n_backups) && is.finite(n_backups)){
        backups <- rev(sort(self$backups))
        to_remove <- backups[(n_backups + 1):length(backups)]

      } else if (is_Date(n_backups)){
        sel <- which(parse_date(self$backup_matrix[, "sfx", drop = FALSE]) < n_backups)
        to_remove <- apply(self$backup_matrix[sel, , drop = FALSE], 1, paste, collapse = ".")

      } else if (is.character(n_backups)){
        dates    <- parse_date(self$backup_matrix[, "sfx", drop = FALSE])
        interval <- parse_interval(n_backups)

        if (identical(interval[["unit"]], "year")){
          limit <- dint::first_of_year(dint::get_year(max(dates)) - interval$value + 1L)
          sel <- dates < limit

        } else if (identical(interval[["unit"]], "month")) {
          limit <- dint::first_of_month(dint::as_date_ym(max(dates)) - interval$value + 1L)
          sel <- dates < limit

        } else if (identical(interval[["unit"]], "week")){
          limit <- dint::first_of_isoweek(dint::as_date_yw(max(dates)) - interval$value + 1L)
          sel <- dates < limit

        } else if (identical(interval[["unit"]], "day")){
          limit <- max(dates) - interval$value + 1L
          sel <- dates < limit
        }

        to_remove <- apply(self$backup_matrix[sel, , drop = FALSE], 1, paste, collapse = ".")

      } else {

        stop("Invalid 'max backups'")
      }

      file.remove(to_remove)
      self
    }
  ),

  active = list(
    backup_matrix = function(){
      if (!length(self$backups))
        return(character())

      res <- super$backup_matrix
      res[order(res[, "sfx"],  decreasing = TRUE), , drop = FALSE]
    },

    backups = function(){
      potential_backups <-
        list.files(self$backup_dir, full.names = self$backup_dir != ".")

      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)
      date_patterns <- c(
        "\\d{4}-\\d{2}-\\d{2}",
        "\\d{4}-\\d{2}",
        "\\d{8}",
        "\\d{6}",
        "\\d{4}"
      )
      date_patterns <- paste0("(", date_patterns, ")", collapse = "|")


      if (is_blank(ext)){
        pat = paste0(name, "^\\.$s(\\..*){0,1}$", date_patterns)

      } else {
        pat <- sprintf("^%s\\.%s\\.%s\\.*", name, date_patterns, ext)
      }

      sort(grep(pat, potential_backups, value = TRUE))
    }
  )
)

