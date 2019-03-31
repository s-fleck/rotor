#' Title
#'
#' @param file
#' @param max_backups
#' @param backups
#'
#' @return
#' @export
#'
#' @examples
#' td <- file.path(tempdir())
#' owd <- setwd(td)
#'
#' # prune tail is useful for integer based indices
#' file.create("foo.log")
#' backup("foo.log")
#' backup("foo.log")
#' backup("foo.log")
#'
#' find_backups("foo.log")
#' prune_backups_tail("foo.log", 2)
#'
#' # cleanup
#' invisible(file.remove(find_backups("foo.log")))
#'
#'
#' # prune head is useful for date based indices
#' # manually override timestamp for the sake of this example
#' backup_date("foo.log", time = as.Date("2019-01-01"))
#' backup_date("foo.log", time = as.Date("2019-01-02"))
#' backup_date("foo.log", time = as.Date("2019-01-03"))
#' find_backups("foo.log")
#' prune_backups_head("foo.log", 2)
#'
#'
#' # cleanup
#' invisible(file.remove(find_backups("foo.log")))  # cleanup
#' invisible(file.remove("foo.log"))
#' if (!length(list.files(td)) unlink(td, recursive = TRUE)
#' setwd(owd)
prune_backups_head <- function(
  file,
  max_backups,
  backups = find_backups(file)
){
  sort(prune_backups(
    file = file,
    max_backups = max_backups,
    sort(backups, decreasing = TRUE)
  ))
}




#' Title
#'
#' @param file
#' @param max_backups
#' @param backups
#'
#' @return
#' @export
#'
#' @examples
prune_backups_tail <- function(
  file,
  max_backups,
  backups = find_backups(file)
){
  sort(prune_backups(
    file = file,
    max_backups = max_backups,
    sort(backups)
  ))
}



#' Prune list of files down to `max_backups`.
#' You must ensure that `backups` is sorted as as you want it already!
#'
#' @param file
#' @param max_backups
#' @param backups
#'
#' @return `character` vector of the names of the remaining backups
#' @noRd
prune_backups <- function(
  file,
  max_backups,
  backups
){
  assert(is_scalar_character(file) && file.exists(file))
  assert(is_scalar_integerish(max_backups) && !is.na(max_backups))
  assert(
    all(file.exists(backups)),
    "Cannot prune backups because the following files do not exist: \n",
    paste(" -", backups[!file.exists(backups)], collapse = "\n")
  )

  to_remove <- backups[(max_backups + 1):length(backups)]
  file.remove(to_remove)
  backups[1:max_backups]
}
