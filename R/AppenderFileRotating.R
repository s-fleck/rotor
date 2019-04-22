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
      min_size = 1,
      n_backups = inf,
      compression = FALSE,
      prerotate = prerotate,
      postrotate = postrotate,
      overwrite = FALSE,
      create_file = TRUE
    ){
      self$set_file(file)
      self$set_threshold(threshold)
      self$set_layout(layout)
      self$set_filters(filters)

      self$set_timestamp_fmt(timestamp_fmt)
      self$set_age(age)
      self$set_min_size(min_size)
      self$set_n_backups(n_backups)
      self$set_prerotate(prerotate)
      self$set_postrotate(postrotate)
      self$set_overwrite(overwrite)
      self$set_create_file(create_file)
    },

    rotate = function(
      age,
      format
    ){
      rotate_date(
        self$file,
        age = age,
        format = self$timestamp_fmt,
        min_size = 1,
        n_backups = inf,
        compression = FALSE,
        prerotate = prerotate,
        postrotate = postrotate,
        overwrite = FALSE,
        create_file = TRUE,
        dry_run = getOption("rotor.dry_run", FALSE),
        verbose = getOption("rotor.dry_run", dry_run)
      )
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

    set_min_size = function(
      x
    ){
      private[[".min_size"]] <- x
      self
    },

    set_n_backups = function(
      x
    ){
      assert(is_n0(x))
      private[[".timestamp_fmt"]] <- x
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
    timestamp_fmt = function() self[[".timestamp_fmt"]]
  ),

  private = list(
    .age = NULL,
    .timestamp_fmt = NULL,
    .min_size = NULL,
    .n_backups = NULL,
    .compression = NULL,
    .prerotate = NULL,
    .postrotate = NULL,
    .overwrite = NULL
  )
)

