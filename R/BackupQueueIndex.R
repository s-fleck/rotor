BackupQueueIndex <- R6::R6Class(
  "BackupQueue",
  inherit = BackupQueue,
  public = list(
    prune = function(n_backups){
      if (self$n_backups < 1)
        return(self)

      to_keep   <- self$backups$path[seq_len(n_backups)]
      to_remove <- setdiff(self$backups$path, to_keep)
      assert(all(file.remove(to_remove)))
      self$pad_index()
    },


    push_backup = function(
      compression = FALSE
    ){
      self$increment_index()

      # generate new filename
      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)
      sfx <- "1"
      if (is_blank(ext)) {
        name_new <- paste(name, sfx, sep = ".")
      } else {
        name_new <- paste(name, sfx, ext, sep = ".")
      }

      file.copy(self$file, name_new, overwrite = FALSE)
      name_new <- compress_and_remove(name_new, compression = compression)
      self$pad_index()
    },


    pad_index = function(){
      if (!length(self$backups$path))
        return(self)

      backups <- self$backups
      backups$sfx_new <- pad_left(backups$index, pad = "0")
      backups$path_new <-
        paste(file.path(backups$dir, backups$name), backups$sfx_new, backups$ext, sep = ".")

      backups$path_new <- gsub("\\.$", "", backups$path_new)

      file.rename(backups$path, backups$path_new)
      self
    },


    increment_index = function(n = 1){
      assert(self$n_backups > 0)
      assert(is_scalar_integerish(n))

      backups <- self$backups

      backups$index <- backups$index + as.integer(n)
      backups$path_new <- paste(
        file.path(backups$dir, backups$name),
        pad_left(backups$index, pad = "0"),
        backups$ext,
        sep = "."
      )
      backups$path_new <- gsub("\\.$", "", backups$path_new)
      file.rename(rev(backups$path), rev(backups$path_new))

      self
    }

  ),

  active = list(
    backups = function(){
      res <- super$backups

      if (nrow(res) < 1){
        return(data.frame())
      }
      res <- res[grep("^\\d+$", res$sfx), ]
      res$index <- as.integer(res$sfx)

      res[order(res$sfx, decreasing = FALSE), ]
    }
  )
)

