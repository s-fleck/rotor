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
      to_keep   <- self$backups[seq_len(n_backups)]
      to_remove <- setdiff(self$backups, to_keep)
      assert(all(file.remove(to_remove)))
      self
    },

    print = function(){
      cat(fmt_class(class(self)[[1]]), "\n\n")

      ori <- file.info(self$file)
      bus <- file.info(self$backups)
      info <- data.frame(
        file = c(row.names(ori), row.names(bus)),
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
      length(self$backups) > 0
    },

    backups = function(){
      potential_backups <-
        list.files(self$backup_dir, full.names = self$backup_dir != ".")

      name <- tools::file_path_sans_ext(self$file)
      ext  <- tools::file_ext(self$file)

      if (is_blank(ext)){
        pat = paste0(name, "\\.[^.]+(\\..*){0,1}$")

      } else {
        pat <- sprintf("^%s\\..*\\.%s\\.*", name, ext)
      }

      sort(grep(pat, potential_backups, value = TRUE))
    },

    backup_matrix = function(){
        if (!length(self$backups))
          return(character())

        name <- tools::file_path_sans_ext(self$file)
        ext  <- tools::file_ext(self$file)

      # identify name parts
        name_end <- attr(gregexpr(name, self$backups[[1]])[[1]], "match.length") + 1L
        a <- strsplit_at_pos(self$backups, name_end)

        if (!is_blank(ext)){
          ext_start <- unlist(gregexpr(ext, a[, 2]))
          b <- strsplit_at_pos(a[, 2], ext_start - 1L)
          res <- cbind(a[, 1], b)
          colnames(res) <- c("name", "sfx", "ext")
        } else {
          res <- a
          colnames(res) <- c("name", "sfx")
        }

        assert(is.matrix(res))
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
