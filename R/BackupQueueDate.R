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




parse_date <- function(x){
  if (is_Date(x)){
    return(x)
  } else if (!is.character(x) && !is_integerish(x)) {
    stop(
      "`", deparse(substitute(x)), "` must be a character or Date, ",
      "not ", preview_object(x), call. = FALSE
    )
  }

  x <- standardize_date_stamp(x)
  dd <- prep_ymd(x)
  res <- as.Date(x)

  assert(!anyNA(res))
  res
}
