BackupQueueIndex <- R6::R6Class(
  "BackupQueue",
  inherit = BackupQueue,
  public = list(
    prune = function(max_backups){
      to_keep   <- self$backups[seq_len(max_backups)]
      to_remove <- setdiff(self$backups, to_keep)
      assert(all(file.remove(to_remove)))
      self$pad_index()
    },


    backup = function(
      compression = FALSE
    ){
      self$push_index()

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
      if (!length(self$backups))
        return(self)

      backups_new <- self$backup_matrix
      assert(is.matrix(backups_new))

      backups_new[, "sfx"] <- as.integer(backups_new[, "sfx"])
      backups_new[, "sfx"] <- pad_left(backups_new[, "sfx"], pad = "0")
      backups_new <- apply(backups_new, 1, paste, collapse = ".")
      backups_old <- apply(self$backup_matrix, 1, paste, collapse = ".")

      file.rename(backups_old, backups_new)
      self
    },


    push_index = function(n = 1){
      backups_new <- self$backup_matrix
      assert(is.matrix(backups_new))

      backups_new[, "sfx"] <- as.integer(backups_new[, "sfx"]) + n
      backups_new <- apply(backups_new, 1, paste, collapse = ".")
      file.rename(rev(self$backups), rev(backups_new))

      self$pad_index()
    }

  ),

  active = list(
    backup_matrix = function(){
      if (!length(self$backups))
        return(character())

      res <- super$backup_matrix
      res[order(res[, "sfx"]), ,drop = FALSE]
    },

    backups = function(){
      potential_backups <-
        list.files(self$backup_dir, full.names = self$backup_dir != ".")

      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)

      if (is_blank(ext)){
        pat = paste0(name, "\\.\\d+(\\..*){0,1}$")

      } else {
        pat <- sprintf("^%s\\.\\d+\\.%s\\.*", name, ext)
      }

      sort(grep(pat, potential_backups, value = TRUE))
    }
  )
)

