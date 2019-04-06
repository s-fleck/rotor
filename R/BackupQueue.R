BackupQueue <- R6::R6Class(
  "BackupQueue",
  public = list(
    initialize = function(
      file,
      backup_dir = dirname(file)
    ){
      self$file <- file
      self$backup_dir <- backup_dir
      self
    },
    file = NULL,
    backup_dir = NULL,


    prune = function(n_backups){
      if (n_backups > 0){
        warning(
          "Pruning a generic BackupQueue with `n_backups > 0` is not",
          "recommended, because it is not defined which backups will be",
          "deleted. Use BackupQueueIndex or BackupQueueDate instead.")
      }
      to_keep   <- self$backups$path[seq_len(n_backups)]
      to_remove <- setdiff(self$backups$path, to_keep)
      assert(all(file.remove(to_remove)))
      self
    },


    print = function(){
      cat(fmt_class(class(self)[[1]]), "\n\n")

      ori <- file.info(self$file)
      bus <- self$backups

      info <- data.frame(
        file = c(row.names(ori), bus$path),
        size = c(ori$size, bus$size)
      )

      dd <- as.matrix(info)


      if (nrow(dd) == 1){
        dd[, "size"] <- readable_size(dd[, "size"])
        dd <- rbind(
          dd,
          c("[no backups]", "")
        )
        dd[, "size"] <- pad_left(dd[, "size"], max(nchar(dd[, "size"])) + 2)
        dd[, "file"] <- pad_right(dd[, "file"])
        assert(nrow(dd) == 2)
        dd[2, ] <-  apply(dd[2, ,drop = FALSE], 1:2, style_subtle)

      } else if (nrow(dd) > 1){
        dd <- rbind(
          dd,
          c(paste(nrow(dd), "files total"), sum(as.integer(dd[, "size"])))
        )
        dd[, "size"] <- pad_left(readable_size(dd[, "size"]))
        dd[, "file"] <- pad_right(dd[, "file"])
        assert(nrow(dd) >= 3)
        sel <- 2:(nrow(dd) - 1)
        dd[sel, ] <-  apply(dd[sel, ,drop = FALSE], 1:2, style_subtle)
      } else {
        stop("Error while printing backup queue. Please file an issue.")
      }

      apply(dd, 1, cat, "\n")
      invisible(self)
    }
  ),


  active = list(
    has_backups = function(){
      self$n_backups > 0
    },


    n_backups = function(){
      nrow(self$backups)
    },


    backups = function(){
      backup_files <- get_backups(
        file = self$file,
        potential_backups =
          list.files(self$backup_dir, full.names = self$backup_dir != "."),
        sfx_patterns = c(
          "\\d+",
          "\\d{4}-\\d{2}-\\d{2}",
          "\\d{4}-\\d{2}"
        )
      )

      # parse to df
      if (!length(backup_files)){
        return(data.frame())
      }

      fname_matrix <- filenames_as_matrix(self$file, backups = backup_files)
      fname_df <- data.frame(
        dir   = dirname(fname_matrix[, "name"]),
        name  = basename(fname_matrix[, "name"]),
        sfx   = fname_matrix[, "sfx"],
        ext   = fname_matrix[, "ext"],
        stringsAsFactors = FALSE
      )
      finfo <- file.info(backup_files)

      res <- cbind(
        data.frame(path = row.names(finfo), stringsAsFactors = FALSE),
        fname_df,
        finfo
      )
      row.names(res) <- NULL

      res
    }
  )
)



readable_size <- function(
  x
){
  sapply(as.numeric(x), utils:::format.object_size, "auto")
}


# public static String readableFileSize(long size) {
#   if(size <= 0) return "0";
#   final String[] units = new String[] { "B", "kB", "MB", "GB", "TB" };
#   int digitGroups = (int) (Math.log10(size)/Math.log10(1024));
#   return new DecimalFormat("#,##0.#").format(size/Math.pow(1024, digitGroups)) + " " + units[digitGroups];
# }
