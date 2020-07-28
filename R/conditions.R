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




NotImplementedError <- function(
  message = sprintf("functionality is not yet implemented", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "NotImplementedError"), call = call)
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
