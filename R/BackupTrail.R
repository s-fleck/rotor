BackupTrail <- R6::R6Class(
  "BackupTrail",
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file)
    ){
      self$file <- file
      self$backup_dir <- backup_dir
      self
    },
    file = NULL,
    backup_dir = NULL
  ),

  active = list(
      backup_matrix = function(){
        if (!length(self$backups))
          return(character())

        name <- tools::file_path_sans_ext(self$file)
        ext  <- tools::file_ext(self$file)

      # identify name parts
        name_end <- attr(gregexpr(name, self$backups[[1]])[[1]], "match.length") + 1L
        a <- strsplit_at_pos(self$backups, name_end)

        if (!is_blank(ext)){
          ext_start <- gregexpr(ext, a[, 2][[1]])[[1]]
          b <- strsplit_at_pos(a[, 2], ext_start - 1L)
          res <- cbind(a[, 1], b)
          colnames(res) <- c("name", "sfx", "ext")
        } else {
          res <- a
          colnames(res) <- c("name", "sfx")
        }
        res
      }
  )
)



BackupTrailIndexed <- R6::R6Class(
  "BackupTrail",
  inherit = BackupTrail,
  public = list(
    prune = function(max_backups){
      to_remove <- self$backups[(max_backups + 1):length(self$backups)]
      file.remove(to_remove)
      self
    }
  ),

  active = list(
    backup_matrix = function(){
      if (!length(self$backups))
        return(character())

      res <- super$backup_matrix
      res[order(res[, "sfx"]), ]
    },

    backups = function(){
      potential_backups <-
        list.files(self$backup_dir, full.names = self$backup_dir != ".")

      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)

      if (is_blank(ext)){
        pat = paste0(name, "\\.\\d+(\\..*){0,1}$")

      } else {
        pat <- sprintf("^%s\\.\\d+\\.%s\\.*", name, ext)
      }

      sort(grep(pat, potential_backups, value = TRUE))
    }
  )
)





BackupTrailDate <- R6::R6Class(
  "BackupTrail",
  inherit = BackupTrail,
  public = list(
    prune = function(
      max_backups
    ){
      assert(is_scalar(max_backups))

      if (is_integerish(max_backups)){
        backups <- rev(sort(self$backups))
        to_remove <- backups[(max_backups + 1):length(backups)]

      } else if (is_Date(max_backups)){
        sel <- which(parse_date(self$backup_matrix[, "sfx", drop = FALSE]) < max_backups)
        to_remove <- apply(self$backup_matrix[sel, , drop = FALSE], 1, paste, collapse = ".")

      } else if (is.character(max_backups)){
        dates    <- parse_date(self$backup_matrix[, "sfx", drop = FALSE])
        interval <- parse_interval(max_backups)

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
      res[order(res[, "sfx"],  decreasing = TRUE), ]
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



get_year <- function(x){
    as.POSIXlt(x, tz = tz(x))$year + 1900L
}



get_month <- function(x){
  as.POSIXlt(x, tz = tz(x))$mon + 1L
}
