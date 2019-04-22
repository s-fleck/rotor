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
  compression  = FALSE,
  add_ext = TRUE,
  overwrite = overwrite,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  stopifnot(
    is_scalar_character(file),
    is_scalar_character(outname),
    file.exists(file),
    is_scalar_logical(add_ext),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose)
  )

  assert(
    is_scalar_atomic(compression) && (
      compression %in% c("base::zip", "zip::zipr") ||
      compression %in% 1:9 ||
      is_bool(compression)
    ),
    '`compression` must be `TRUE`, `FALSE`, or an integer between 1 and 9',
    'or the character scalers "base::zip" or "zip::zipr" not: ',
    preview_object(compression)
  )


  # init
    if (isTRUE(compression)){
      compression <-
        {if (requireNamespace("zip", quietly = TRUE)) "zip::zipr" else "base::zip"}
    }

    if (!isFALSE(compression))
      outname <- paste0(outname, ".zip")

    if (file.exists(outname)){
      if (overwrite){
        file_remove(outname, dry_run = dry_run, verbose = verbose)
      } else {
        stop(sprintf("Backup '%s' exists and `overwrite == FALSE`", outname))
      }
    }


  # copy
    if (isFALSE(compression)){
      file_copy(file, outname, overwrite = overwrite, dry_run = dry_run, verbose = verbose)
      return(outname)
    }

  if (compression %in% 1:9){
    compression_level <- compression
    compression <- "zip::zipr"
  } else {
    compression_level <- 9
  }

  # zip
    if (identical(compression, "zip::zipr")){
      assert_namespace("zip")
      msg_file_copy(file, outname, dry_run = dry_run, verbose = verbose)
      if (!dry_run) zip::zipr(outname, file)

    } else if (identical(compression, "base::zip")){
      owd <- setwd(dir = dirname(file))
      on.exit(setwd(owd))
      utils::zip(basename(outname), files = basename(file), flags="-q")

    } else {
      stop("should not be possible to arrive here")
    }

  outname
}
