NotImplementedError <- function(
  message = "function not implemented",
  ...,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "NotImplementedError"))
}



ValueError <- function(
  message,
  ...,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "ValueError"))
}



DirIsNotEmptyError <- function(
  message = sprintf("directory '%s' does not exist.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "DirIsNotEmptyError"), call = call)
}



DirDoesNotExistError <- function(
  message = sprintf("directory '%s' does not exist.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "DirDoesNotExistError"), call = call)
}



PathIsNotADirError <- function(
  message = sprintf("'%s' is not a directory.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "PathIsNotADirError"), call = call)
}



NotImplementedError <- function(
  message = sprintf("functionality is not yet implemented", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "NotImplementedError"), call = call)
}




ObjectHasNotChangedMessage <- function(message, ..., class = NULL, call = NULL){
  condition(
    message = as.character(message),
      call = call,
      class = union(class, c("ObjectHasNotChangedMessage", "message"))
  )
}




condition <- function(message, ..., class = NULL, call = NULL){
  structure(
    list(
      message = as.character(message),
      call = call,
      ...),
    class = union(class, c("condition"))
  )
}




error <- function(message, ..., class = NULL, call = NULL){
  structure(
    list(
      message = as.character(message),
      call = call,
      ...),
    class = union(class, c("error", "condition"))
  )
}
