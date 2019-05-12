last <- function(x){
  x[[length(x)]]
}




first <- function(x){
  x[[1]]
}




# rename a file by incrementing a timestamp in the filename. very narrow function
# only used during unit tests
replace_date_stamp <- function(
  x,
  increment = NULL,
  replace = NULL
){
  if (is.character(replace)) replace <- as.Date(replace)
  assert(is.null(increment) || is_scalar_integerish(increment))
  assert(is.null(replace)   || is_scalar_Date(replace))
  assert(is.null(increment) + is.null(replace) == 1)

  splt <- strsplit(x, ".", fixed = TRUE)

  assert(all(sapply(splt, length) == 3))

  splt <- lapply(splt, function(.x) {
    .x <- as.list(.x)
    .x[[2]] <- parse_date(.x[[2]])
    data.frame(
      name = .x[[1]],
      sfx  = .x[[2]],
      ext  = .x[[3]]
    )
  })

  splt <- do.call(rbind, splt)

  if (!is.null(increment)){
    splt$sfx <- splt$sfx + increment

  } else if (!is.null(replace)){
    splt$sfx <- replace
  }

  new_name <- apply(splt, 1, paste, collapse = ".")

  assert(all(file_exists(x)))
  assert(isTRUE(!file_exists(new_name)))
  file_rename(x, new_name)
  new_name
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
  assert(all(substr(x, pos, pos) %in% seps))
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




is_windows <- function(){
  Sys.info()["sysname"] == "Windows"
}
