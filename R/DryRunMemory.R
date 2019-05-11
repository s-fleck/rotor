DryRunMemory <- R6::R6Class(
  "DryRunMemory",
  public = list(
    initialize = function(){
      self$memory <- list()
    },

    list_files = function(path){
      x <- path_tidy(list.files(path, full.names = TRUE))
      self$fake_out(x)
    },

    fake_out = function(x){

      xt <- path_tidy(x)

      for (op in self$memory){
        if (identical(op[["type"]], "create")){
          x <- c(x, op$file)
          xt <- path_tidy(x)

        } else if (identical(op[["type"]], "delete")){
          x <- x[!xt %in% op[["file"]]]
          xt <- x

        } else if (identical(op[["type"]], "move")){
          x[xt %in% op[["from"]]] <- op[["to"]]
          xt <- path_tidy(x)
        }
      }

      x
    },

    memory = NULL,

    create = function(file){
      assert(is_scalar_character(file))
      file <- path_tidy(file)
      self$memory <- c(self$memory, list(list(type = "create", file = file)))
      self
    },

    delete = function(file){
      assert(is_scalar_character(file))
      file <- path_tidy(file)
      self$memory <- c(self$memory, list(list(type = "delete", file = file)))
      self
    },

    move = function(from, to){
      assert(is_scalar_character(from))
      assert(is_scalar_character(to))
      from <- path_tidy(from)
      to <- path_tidy(to)

      self$memory <- c(self$memory, list(list(type = "move", from = from, to = to)))
      self
    }
  )
)
