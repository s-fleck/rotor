#' A Superclass
#' @field foo returns "bar"
ex <- R6::R6Class(
  "ex",
  active = list(foo = function() "bar")
)


#' A Subclass
#' @field foo returns "bar"
sub <- R6::R6Class(
  "sub",
  inherit = ex
)
