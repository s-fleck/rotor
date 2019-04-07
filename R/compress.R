#' Compress and remove a file
#'
#' Internal helper function used in [backup()]
#'
#' @param file
#' @param compression
#' @param compression_level
#' @param remove_file
#'
#' @return `character` scalar: path to the compressed file
#'
compress_and_remove <- function(
  file,
  compression  = TRUE,
  compression_level = 9,
  remove = TRUE,
  dry_run = getOption("rotor.dry_run", FALSE),
  verbose = getOption("rotor.dry_run", dry_run)
){
  stopifnot(
    is_scalar_character(file),
    file.exists(file),
    is_scalar_character(compression) || is_scalar_logical(compression),
    is_scalar_integerish(compression_level),
    is_scalar_logical(remove),
    is_scalar_logical(dry_run),
    is_scalar_logical(verbose)
  )

  if (isFALSE(compression)){
    return(file)
  }

  if (isTRUE(compression)){
    compression <- {
      if (requireNamespace("zip", quietly = TRUE)) "zip" else "zip_base"
    }
  }

  # zip
  out <- paste0(file, ".zip")
  assert(!file.exists(out))
  if (verbose)   message("'", file, "' -> '", out, "'")

  if (identical(compression, "zip")){
    assert_namespace("zip")
    if (!dry_run)  zip::zipr(out, file, compression_level = compression_level)

  } else if (identical(compression, "zip_base")){
    owd <- setwd(dir = dirname(file))
    on.exit(setwd(owd))
    utils::zip(basename(out), files = basename(file), flags="-q")
  }

  if (remove && !dry_run){
    if (verbose) message("removing '", file, "'")
    assert(file.exists(out))
    unlink(file, recursive = TRUE)
  }

  out
}
