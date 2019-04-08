BackupQueueDate <- R6::R6Class(
  "BackupQueue",
  inherit = BackupQueue,
  public = list(
    push_backup = function(
      format = "%Y-%m-%d",
      compression = FALSE,
      overwrite = FALSE,
      now = Sys.Date(),
      dry_run = getOption("rotor.dry_run", FALSE),
      verbose = getOption("rotor.dry_run", dry_run)
    ){
      stopifnot(
        is_scalar_character(format),
        is_scalar_logical(compression),
        is_scalar_logical(overwrite),
        is_scalar_Date(now),
        is_scalar_logical(dry_run),
        is_scalar_logical(verbose)
      )

      # generate new filename
      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)
      sfx  <- format(now, format = format)

      if (is_blank(ext)) {
        name_new <- paste(name, sfx, sep = ".")
      } else {
        name_new <- paste(name, sfx, ext, sep = ".")
      }

      copy_or_compress(
        self$file,
        outname = name_new,
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

      # no backups -> do nothing
      if (!self$has_backups){
        if (verbose) message("Nothing to prune; no backups found for '", self$file, "'")
        return(self)
      }

      if (is.infinite(n_backups) || is.na(n_backups)){
        if (verbose) message("Nothing to prune; `n_backups` is set to", format(n_backups))
        return(self)
      }


      if (is_integerish(n_backups) && is.finite(n_backups)){
        # prune based on number of backups
        backups   <- rev(sort(self$backups$path))
        to_remove <- backups[(n_backups + 1):length(backups)]

      } else {
        # prune based on dates and intervals
        if (is.character(n_backups) && is_parsable_date(n_backups)){
          # Date like strings
          limit <- parse_date(n_backups)

        } else if (is_Date(n_backups)){
          # true Dates
          limit <- n_backups

        } else if (is_parsable_interval(n_backups)){
          # interval like strings
          interval <- parse_interval(n_backups)

          if (identical(interval[["unit"]], "year")){
            limit <- dint::first_of_year(dint::get_year(self$last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "quarter")){
            limit <- dint::first_of_quarter(dint::as_date_yq(self$last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "month")) {
            limit <- dint::first_of_month(dint::as_date_ym(self$last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "week")){
            limit <- dint::first_of_isoweek(dint::as_date_yw(self$last_backup) - interval$value + 1L)

          } else if (identical(interval[["unit"]], "day")){
            limit <- self$last_backup - interval$value + 1L
          }
        }
      to_remove <- self$backups$path[self$backups$date < limit]
    }

    msg_prune_backups(self$file, to_remove, dry_run, verbose)

    if (dry_run){
      self
    } else {
      assert(all(file.remove(to_remove)))
      self
    }
  }),

  active = list(

    last_backup = function(){
      max(self$backups$date)
    },

    backups = function(){
      res <- super$backups

      if (nrow(res) < 1){
        return(data.frame())
      }

      sfx_patterns = c(
        "\\d{4}-\\d{2}-\\d{2}",
        "\\d{4}-\\d{2}",
        "\\d{8}",
        "\\d{6}",
        "\\d{4}"
      )
      sfx_patterns <- paste0("(", sfx_patterns, ")", collapse = "|")

      res <- res[grep(sfx_patterns, res$sfx), ]
      res$date <- parse_date(res$sfx)

      res[order(res$date, decreasing = TRUE), ]
    }
  )
)
