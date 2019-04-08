#' Compress and remove a file
#'
#' Internal helper function used in [backup()]
#'
#' @param file `character` scalar. File to copy/compress
#' @param compression `logical` or `character` scalar. Valid values are
#'   `TRUE`, `FALSE`, `"zip"` for [zip::zipr()] or `"base::zip"` for [base::zip()]
#' @param outname `scalar` `character` scalar. name of the target file.
#' @param add_ext `logical` scalar. add `.zip` extensions to `outname` when
#'   compressing
#' @param compression_level passed on to [zip::zipr()] (not if using [base::zip()])
#'
#' @noRd
#'
#' @return `character` scalar: path to the compressed file
#'
copy_or_compress <- function(
  file,
  outname,
  compression  = FALSE,
  compression_level = 9,
  add_ext = TRUE,
  overwrite = overwrite,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  stopifnot(
    is_scalar_character(file),
    is_scalar_character(outname),
    file.exists(file),
    is_scalar_character(compression) || is_scalar_logical(compression),
    is_scalar_integerish(compression_level),
    is_scalar_logical(add_ext),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose)
  )

  # init
    if (isTRUE(compression)){
      compression <-
        {if (requireNamespace("zip", quietly = TRUE)) "zip" else "base::zip"}
    }

    if (!isFALSE(compression))
      outname <- paste0(outname, ".zip")

    if (file.exists(outname)){
      if (overwrite && !dry_run){
        msg_file_remove(outname, dry_run, verbose)
        file.remove(outname)
      } else {
        stop("Backup exists and `overwrite == FALSE`")
      }
    }

    msg_file_copy(file, outname, dry_run, verbose)

  # copy
    if (isFALSE(compression) && !dry_run){
      file.copy(file, outname, overwrite = overwrite)
      return(outname)
    }

  # zip
    if (identical(compression, "zip")){
      assert_namespace("zip")
      if (!dry_run) zip::zipr(outname, file, compression_level = compression_level)

    } else if (identical(compression, "base::zip")){
      owd <- setwd(dir = dirname(file))
      on.exit(setwd(owd))
      utils::zip(basename(outname), files = basename(file), flags="-q")
    }

  outname
}
