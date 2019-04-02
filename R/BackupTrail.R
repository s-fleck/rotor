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
      backup_matrix = function(){
        if (!length(self$backups))
          return(character())

        name <- tools::file_path_sans_ext(self$file)
        ext  <- tools::file_ext(self$file)

      # identify name parts
        name_end <- attr(gregexpr(name, self$backups[[1]])[[1]], "match.length") + 1L
        a <- strsplit_at_pos(self$backups, name_end)

        if (!is_blank(ext)){
          ext_start <- gregexpr(ext, a[, 2][[1]])[[1]]
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
