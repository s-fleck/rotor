DryRunMemory <- R6::R6Class(
  "DryRunMemory",
  public = list(
    initialize = function(){
      self$reset()
      self$active <- FALSE
      self
    },

    # activate/deactivate
    active = NULL,

    activate = function(...){
      self$active <- TRUE
      self$reset()
      self
    },

    deactivate = function(){
      self$active <- FALSE
      self$reset()
      self
    },


    list = function(path){
      x <- path_standardize(list.files(path, full.names = TRUE))
      unique(self$fake(x))
    },


    # file operations
    exists = function(...){
      files <- c(...)

      if (path_standardize(files) %in% self$memory$file){
        path_standardize(files) %in% self$fake(files)
      } else {
        file.exists(files)
      }
    },

    fake = function(x){

      xt <- path_standardize(x)

      for (i in seq_len(nrow(self$memory))){
        op <- self$memory[i, ]

        if (identical(op$op, "create")){
          x <- c(x, op$file)
          xt <- path_standardize(x)

        } else if (identical(op$op, "remove")){
          x <- x[!xt %in% op$file]
          xt <- x
        }
      }

      x
    },

    memory = NULL,

    reset = function(){
      self$memory <- data.frame(
        op = character(),
        file = character(),
        stringsAsFactors = FALSE
      )
    },

    create = function(file){
      assert(is.character(file))
      file <- path_standardize(file)
      self$memory <- rbind(
        self$memory,
        data.frame(
          op = "create",
          file = file,
          stringsAsFactors = FALSE
        )
      )
      rep(TRUE, length(file))
    },


    remove = function(file){
      assert(is.character(file))
      file <- path_standardize(file)
      self$memory <- rbind(
        self$memory,
        data.frame(
          op = "remove",
          file = file,
          stringsAsFactors = FALSE
        )
      )
      rep(TRUE, length(file))
    },

    move = function(from, to){
      assert(is.character(from))
      assert(is.character(to))
      from <- path_standardize(from)
      to <- path_standardize(to)

      self$remove(from)
      self$create(to)
      rep(TRUE, length(from))
    }
  )
)
