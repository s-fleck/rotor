not_implemented_error <- function(
  message = "function not implemented",
  ...,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "value_error"))
}



value_error <- function(
  message,
  ...,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "value_error"))
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
