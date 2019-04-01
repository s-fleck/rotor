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
    prune = function(max_backups){
      to_remove <- self$backups[(max_backups + 1):length(self$backups)]
      file.remove(to_remove)
      self
    }
  ),

  active = list(
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
