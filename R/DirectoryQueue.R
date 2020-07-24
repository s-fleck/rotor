# CacheQueue --------------------------------------------------------------

#' An R6 class for managing a persistent file-based queue
#'
DirectoryQueue <- R6::R6Class(
  "DirectoryQueue",
  cloneable = FALSE,
  public = list(

    initialize = function(
      ...
    ){
      not_implemented_error()
    },


    push = function(
      x,
      ...
    ){
      not_implemented_error()
    },


    prune = function(
      x,
      ...
    ){
      not_implemented_error()
    },


    # ... setters -------------------------------------------------------------
    set_dir = function(
      x,
      create = TRUE
    ){
      assert(is_scalar_character(x))
      assert(is_scalar_bool(create))
      x <- path_tidy(x)

      if (!file_exists(x)){
        if (create){
          message("creating directory '", x, "'")
          dir.create(x, recursive = TRUE)
        } else {
          stop("directory '", x, "' does not exist")
        }
      }

      assert(is_dir(x), "'", x, "' is not a directory")
      private[[".dir"]] <- x
      self
    }
  ),


  # ... getters -------------------------------------------------------------
  active = list(
    #' @field dir a `character` scalar. path of the directory in which to store
    #'   the cache files
    dir = function(dir){
      if (missing(dir))
        return(get(".dir", envir = private))

      self$set_dir(dir, create = FALSE)
    },


    n = function(){
      nrow(self$files)
    },


    files = function(){

      files <- list.files(self$dir, full.names = TRUE)

      if (!length(files)){
        return(data.frame())
      }

      finfo <- file.info(files)

      res <- cbind(
        data.frame(path = rownames(finfo), stringsAsFactors = FALSE),
        data.frame(key = basename(rownames(finfo)), stringsAsFactors = FALSE),
        finfo
      )
      row.names(res) <- NULL

      res[order(res$mtime), ]
    }
  ),

  private = list(
    .dir = NULL
  )
)
