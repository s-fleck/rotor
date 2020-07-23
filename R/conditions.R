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




error <- function(message, ..., class = NULL, call = NULL){
  structure(
    list(
      message = as.character(message),
      call = call,
      ...),
    class = union(class, c("error", "condition"))
  )
}
