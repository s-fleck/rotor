BackupTrailIndex <- R6::R6Class(
  "BackupTrail",
  inherit = BackupTrail,
  public = list(
    prune = function(max_backups){
      to_remove <- self$backups[(max_backups + 1):length(self$backups)]
      file.remove(to_remove)
      self
    },

    backup = function(){

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

