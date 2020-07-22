last <- function(x){
  x[[length(x)]]
}




first <- function(x){
  x[[1]]
}




fmt_bytes <- function(
  x
){
  x <- as.numeric(x)

  readablifiy <- function(.x){
    for (unit in c("B", "KiB", "MiB", "GiB", "TiB")){
      if (max(abs(.x)) < 1024 || unit == "TiB")
        break
      else
        .x <- .x / 1024
    }
    return(paste(round(.x, 1), unit))
  }

  vapply(x, readablifiy, character(1))
}




expect_snapshot_unchanged <- function(snap){
  if (!length(snap$info$size) && !length(utils::fileSnapshot(snap$path)$info$size))
    testthat::expect_true(TRUE)
  else
    testthat::expect_true(!any(utils::changedFiles(snap)$changes))
}




#' Splits a string at `pos` (removing the character at pos)
#'
#' @param x a `character` vector
#' @param pos an `integer` vector
#'
#' @noRd
strsplit_at_seperator_pos <- function(
  x,
  pos,
  seps = "."
){
  assert(
    all(substr(x, pos, pos) %in% seps),
    "Not all names have a '.' sepparator at pos ", pos, ":\n",
    paste0("* ", x, collapse = "\n")
  )
  matrix(data = c(substr(x, 1, pos - 1L), substr(x, pos + 1L, nchar(x))), ncol = 2)
}




strsplit_at_pos <- function(
  x,
  pos
){
  matrix(data = c(substr(x, 1, pos), substr(x, pos + 1L, nchar(x))), ncol = 2)
}




path_equal <- function(x, y){
  if (identical(x, y)){
    return(TRUE)
  }

  x <- path_tidy(x)
  y <- path_tidy(y)

  if (identical(x, y)){
    return(TRUE)
  }

  x <- path.expand(x)
  y <- path.expand(y)

  identical(x, y)
}




path_setequal <- function(x, y){
  x <- unique(x)
  y <- unique(y)

  if (setequal(x, y)){
    return(TRUE)
  }

  x <- path_tidy(x)
  y <- path_tidy(y)

  if (setequal(x, y)){
    return(TRUE)
  }

  x <- path.expand(x)
  y <- path.expand(y)
  setequal(x, y)
}




expect_path_equal <- function(x, y){
  testthat::expect_true(path_equal(x, y))
}




expect_path_setequal <- function(x, y){
  testthat::expect_true(path_setequal(x, y))
}




#' Clean up paths to make them comparable, inspired by fs::path_tidy
#'
#' @param x `character` vector
#'
#' @return a `character` vector
#' @noRd
path_tidy <- function(x){
  x <- gsub("\\\\", "/", x)
  x <- gsub("(?!^)/+", "/", x, perl = TRUE)

  sel <- x != "/"
  x[sel] <- gsub("/$", "", x[sel])

  sel <- is_windows_path(x)

  if (any(sel)){
    clean_win <- function(.x){
      substr(.x, 1, 1)  <- toupper(substr(.x, 1 ,1))
      .sel <- nchar(.x) == 2
      .x[.sel] <- paste0(.x[.sel], "/")
      .x
    }

    x[sel] <- clean_win(x[sel])
  }

  x
}




path_standardize <- function(x){
  path_tidy(path.expand(x))
}




is_windows_path <- function(x){
  nchar(x) >= 2 & grepl("^[A-Za-z].*", x) & substr(x, 2, 2) == ":"
}




# for R < 3.5
isFALSE <- function(x){
  identical(x, FALSE)
}



expect_dir_empty <- function(
  path
){
  assert(is_scalar_character(path))
  testthat::expect_true(
    dir.exists(path),
    info = sprintf("'%s' does not exist", path)
  )
  testthat::expect_true(
    is_dir(path),
    info = sprintf("'%s' is not a directory", path)
  )
  testthat::expect_true(
    length(list.files(path)) == 0L,
    info = sprintf("Directory '%s' is not empty", path)
  )
}
