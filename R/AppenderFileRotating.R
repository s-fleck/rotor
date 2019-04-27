AppenderFileRotatingDate <- R6::R6Class(
  "AppenderFileRotating",
  inherit = lgr::AppenderFile,
  public = list(
    initialize = function(
      file,
      threshold = NA_integer_,
      layout = lgr::LayoutFormat$new(),
      filters = NULL,
      age = NULL,
      timestamp_fmt = "%Y-%m-%d",
      size = 1,
      max_backups = Inf,
      compression = FALSE,
      prerotate = identity,
      postrotate = identity,
      overwrite = FALSE,
      create_file = TRUE
    ){
      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      self$set_timestamp_fmt(timestamp_fmt)
      self$set_age(age)
      self$set_size(size)
      self$set_max_backups(max_backups)
      self$set_compression(compression)
      self$set_prerotate(prerotate)
      self$set_postrotate(postrotate)
      self$set_overwrite(overwrite)
      self$set_create_file(create_file)

      self
    },

    rotate = function(
      dry_run     = getOption("rotor.dry_run", FALSE),
      verbose     = getOption("rotor.dry_run", dry_run),
      now = Sys.Date()
    ){
      rotate_date(
        self$file,
        age = self$age,
        format      = self$timestamp_fmt,
        size    = self$size,
        max_backups = self$max_backups,
        compression = self$compression,
        prerotate   = self$prerotate,
        postrotate  = self$postrotate,
        overwrite   = self$overwrite,
        create_file = self$create_file,
        now = now,
        dry_run     = dry_run,
        verbose     = verbose
      )

      self
    },


    prune = function(max_backups = self$max_backups){
      BackupQueueDate$new(self$file)$prune(max_backups)
      self
    },

    set_age = function(
      x
    ){
      private[[".age"]] <- x
      self
    },

    set_timestamp_fmt = function(
      x
    ){
      assert(is_valid_date_format(x))
      private[[".timestamp_fmt"]] <- x
      self
    },

    set_size = function(
      x
    ){
      private[[".size"]] <- x
      self
    },

    set_max_backups = function(
      x
    ){
      assert(is.infinite(x) || is_n0(x))
      private[[".max_backups"]] <- x
      self
    },

    set_compression = function(
      x
    ){
      private[[".compression"]] <- x
      self
    },

    set_prerotate = function(
      x
    ){
      assert(is.function(x))
      private[[".prerotate"]] <- x
      self
    },

    set_postrotate = function(
      x
    ){
      assert(is.function(x))
      private[[".postrotate"]] <- x
      self
    },

    set_overwrite = function(
      x
    ){
      assert(is_scalar_logical(x))
      private[[".overwrite"]] <- x
      self
    },

    set_create_file = function(
      x
    ){
      assert(is_scalar_logical(x))
      private[[".create_file"]] <- x
      self
    }

  ),

  active = list(
    age = function() get(".age", private),
    timestamp_fmt = function() get(".timestamp_fmt", private),
    size = function() get(".size", private),
    max_backups = function() get(".max_backups", private),
    compression = function() get(".compression", private),
    prerotate = function() get(".prerotate", private),
    postrotate = function() get(".postrotate", private),
    overwrite = function() get(".overwrite", private),
    create_file = function() get(".create_file", private),

    backups = function(){
      BackupQueueDate$new(self$file)$backups
    }
  ),

  private = list(
    .age = NULL,
    .timestamp_fmt = NULL,
    .size = NULL,
    .max_backups = NULL,
    .compression = NULL,
    .prerotate = NULL,
    .postrotate = NULL,
    .overwrite = NULL,
    .create_file = NULL
  )
)
