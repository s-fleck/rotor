BackupTrail <- R6::R6Class(
  "BackupTrail",
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
    backup_dir = NULL
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
        res
      }
  )
)
