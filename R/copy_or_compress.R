#' Compress and remove a file
#'
#' Internal helper function used in [backup()]
#'
#' @inheritParams rotate_date
#' @param file `character` scalar. File to copy/compress
#' @param outname `scalar` `character` scalar. name of the target file.
#' @param add_ext `logical` scalar. add `.zip` extensions to `outname` when
#'   compressing
#'
#' @noRd
#'
#' @return `character` scalar: path to the compressed file
#'
copy_or_compress <- function(
  file,
  outname,
  compression = FALSE,
  add_ext = TRUE,
  overwrite = FALSE
){
  stopifnot(
    is_scalar_character(file),
    is_scalar_character(outname),
    file_exists(file),
    is_scalar_logical(overwrite),
    is_scalar_logical(add_ext)
  )

  assert(
    is_scalar_atomic(compression) && (
      compression %in% c("utils::zip", "zip::zipr") ||
      compression %in% 1:9 ||
      is_bool(compression)
    ),
    '`compression` must be `TRUE`, `FALSE`, or an integer between 1 and 9',
    'or the character scalers "utils::zip" or "zip::zipr" not: ',
    preview_object(compression)
  )


  # init
    if (isTRUE(compression)){
      compression <- "utils::zip"
    }

    if (!isFALSE(compression))
      outname <- paste0(outname, ".zip")

    if (file_exists(outname)){
      if (overwrite){
        file_remove(outname)
      } else {
        stop(sprintf("File '%s' exists and `overwrite == FALSE`", outname))
      }
    }

    if (compression %in% 1:9){
      compression_level <- compression
      compression <- "zip::zipr"
    } else {
      compression_level <- 9
    }


  # logic
    if (DRY_RUN$active || isFALSE(compression)){
      file_copy(file, outname, overwrite = overwrite)

    } else if (identical(compression, "zip::zipr")){
      assert_namespace("zip")
      msg_file_copy(file, outname)
      zip::zipr(outname, file)

    } else if (identical(compression, "utils::zip")){
      owd <- setwd(dir = dirname(file))
      on.exit(setwd(owd))
      utils::zip(outname, files = basename(file), flags="-q")

    } else {
      stop("should not be possible to arrive here")
    }

  Sys.setFileTime(outname, file.mtime(file))
  outname
}
