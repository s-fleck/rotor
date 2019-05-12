#' @rdname BackupQueue
#' @export
BackupQueueIndex <- R6::R6Class(
  "BackupQueue",
  inherit = BackupQueue,
  public = list(
    prune = function(
      max_backups
    ){
      if (!should_prune(self, max_backups))
        return(self)

      to_keep   <- self$backups$path[seq_len(max_backups)]
      to_remove <- setdiff(self$backups$path, to_keep)

      file_remove(to_remove)
      self$pad_index()
    },


    push_backup = function(
      compression = FALSE
    ){
      assert_valid_compression(compression)
      self$increment_index()

      # generate new filename
      name <- file.path(
        self$backup_dir,
        tools::file_path_sans_ext(basename(self$file))
      )
      ext  <- tools::file_ext(self$file)
      sfx <- "1"
      if (is_blank(ext)) {
        name_new <- paste(name, sfx, sep = ".")
      } else {
        name_new <- paste(name, sfx, ext, sep = ".")
      }

      copy_or_compress(
        self$file,
        outname = name_new,
        compression = compression,
        add_ext = TRUE,
        overwrite = FALSE
      )

      self$pad_index( )
    },


    pad_index = function(
    ){
      if (nrow(self$backups) <= 0)
        return(self)

      backups <- self$backups
      backups$sfx_new <- pad_left(backups$index, pad = "0")
      backups$path_new <-
        paste(file.path(backups$dir, backups$name), backups$sfx_new, backups$ext, sep = ".")

      backups$path_new <- gsub("\\.$", "", backups$path_new)

      file_rename(
        backups$path,
        backups$path_new
      )

      self
    },


    increment_index = function(
      n = 1
    ){
      if (self$n_backups <= 0){
        return(self)
      }
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

      file_rename(
        rev(backups$path),
        rev(backups$path_new)
      )

      self
    }

  ),

  active = list(
    backups = function(){
      res <- super$backups

      if (nrow(res) < 1){
        return(EMPTY_BACKUPS_INDEX)
      }
      res <- res[grep("^\\d+$", res$sfx), ]
      res$index <- as.integer(res$sfx)

      res[order(res$sfx, decreasing = FALSE), ]
    }
  )
)
