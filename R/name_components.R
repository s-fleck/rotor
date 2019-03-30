get_name_components <- function(
  x
){
  bn <- basename(x)

  res <- matrix(
    nrow = length(bn),
    ncol = 5,
    dimnames = list(NULL, c("dir", "name", "ext", "sfx", "arc"))
  )






  sp <- strsplit(bn, ".", fixed = TRUE)


  lapply(sp, function(.x){
    res <- list(
      dir = dirname(.x),
      name = NULL,
      ext = NULL,
      sfx = NULL,
      arc = NULL
    )

    for (i in rev(seq_along(.x))){
      if (is_arc_ext(x[[i]])){
        res[["arc"]] <- x[[i]]
      }







    }




    # Determine Archive Format
      # id tarbals
      if (x[[len - 1L]] == "tar"){
        res[["arc"]] <- paste(.x[[len - 1L]], .x[{len}], sep = ".")

      } else if (is_arc_ext(x[[len]])) {
        res[["arc"]] <- x[[len]]
      }





  })


}


is_integer_sfx <- function(x){
  !is.na(as.integer(x))
}



is_date_sfx <- function(x){

}


is_arc_ext <- function(
  x,
  zip_exts = getOption("rtr.zip_exts")
){
  tolower(x) %in% tolower(zip_exts)
}



is_tarball <- function(x){
  grepl("\\.tar\\.[a-zA-Z0-9]*$", x)
}


get_arc_ext <- function(x){
  ae <- getOption("rtr.arc_exts")
  pat <- paste0("(\\.", ae, "$)", collapse = "|")
  matches <- gregexpr(pat, x)

  vapply(seq_along(x), function(i) {
    start <- matches[[i]][[1]] + 1L
    mlen <- attr(matches[[i]], "match.length")
    substr(x[[i]], start, start + mlen)
  }, character(1))

}
