#' @rdname BackupQueue
#' @export
BackupQueueDate <- R6::R6Class(
  inherit = BackupQueueDateTime,
  "BackupQueueDate",
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file),
      format = "%Y-%m-%d"
    ){
      self$file <- file
      self$backup_dir <- backup_dir
      self$fmt <- format
      self
    }
  ),

  active = list(
    last_backup = function(){
      as.Date(as.character(max(self$backups$timestamp)))
    }
  )
)
