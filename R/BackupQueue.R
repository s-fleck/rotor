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

    print = function(){
      cat(fmt_class(class(self)[[1]]), "\n\n")

      ori <- file.info(self$file)
      bus <- file.info(self$backups)
      info <- data.frame(
        file = c(row.names(ori), row.names(bus)),
        size = c(ori$size, bus$size)
      )

      dd <- as.matrix(info)
      dd <- rbind(
        dd,
        c(paste(nrow(dd), "files total"), sum(as.integer(dd[, "size"])))
      )

      dd[, "file"] <- pad_right(dd[, "file"])
      dd[, "size"] <- pad_left(readable_size(dd[, "size"]))

      dd[2:(nrow(dd) - 1), ] <-  apply(dd[2:(nrow(dd) - 1), ], 1:2, style_subtle)
      dd <- apply(dd, 1, cat, "\n")

      invisible(self)
    }
  ),


  active = list(
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
  utils:::format.object_size(as.numeric(x), "auto")
}


# public static String readableFileSize(long size) {
#   if(size <= 0) return "0";
#   final String[] units = new String[] { "B", "kB", "MB", "GB", "TB" };
#   int digitGroups = (int) (Math.log10(size)/Math.log10(1024));
#   return new DecimalFormat("#,##0.#").format(size/Math.pow(1024, digitGroups)) + " " + units[digitGroups];
# }
