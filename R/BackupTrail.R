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
  active = list(
    backups = function(){
      potential_backups <-
        list.files(self$backup_dir, full.names = self$backup_dir != ".")

      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)

      if (is_blank(ext)){
        pat = paste0(name, "\\.[^.]+(\\..*){0,1}$")

      } else {
        pat <- sprintf("^%s\\..*\\.%s\\.*", name, ext)
      }

      sort(grep(pat, potential_backups, value = TRUE))
    }
  )
)





#' Find backups of `file`
#'
#' @inheritParams get_name_components
#' @export
find_backups <- function(
  file
){
  stopifnot(
    is_scalar_character(file),
    dir.exists(dirname(file))
  )





  get_backups(
    file,
    list.files(dirname(file), full.names = dirname(file) != ".")
  )
}




#' Identify which files are backups of `file`
#'
#' @inheritParams get_name_components
#' @noRd
get_backups <- function(
  file,
  potential_backups
){
  stopifnot(
    is_scalar_character(file),
    is.character(potential_backups)
  )

  name <- tools::file_path_sans_ext(file)
  ext  <- tools::file_ext(file)

  if (is_blank(ext)){
    pat = paste0(name, "\\.[^.]+(\\..*){0,1}$")

  } else {
    pat <- sprintf("^%s\\..*\\.%s\\.*", name, ext)
  }

  sort(grep(pat, potential_backups, value = TRUE))
}
