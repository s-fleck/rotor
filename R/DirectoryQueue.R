# DirectoryQueue --------------------------------------------------------------

#' An R6 class for managing persistent file-based queues (abstract base class)
#'
#' Abstract class from which all other classes in \pkg{rotor} inherit their
#' basic fields and methods.
#'
#' @template r6_api
#' @export
DirectoryQueue <- R6::R6Class(
  "DirectoryQueue",
  cloneable = FALSE,
  public = list(

    initialize = function(
      ...
    ){
      NotImplementedError()
    },


    push = function(
      x,
      ...
    ){
      NotImplementedError()
    },


    prune = function(
      x,
      ...
    ){
      NotImplementedError()
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
          stop(DirDoesNotExistError(dir = x))
        }
      }

      assert(is_dir(x), PathIsNotADirError(dir = x))
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
