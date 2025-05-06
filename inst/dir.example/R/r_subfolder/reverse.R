#' reverse a string
#'
#' @param x string
#'
#' @export
reverse <- function(x) {
  paste(rev(strsplit(x, "")[[1]]), collapse = "")
}
