source_with_context <- function(x) {
  testthat::context_start_file(sub("test-([^/]+)$", "\\1", x))
  source(x, local = TRUE)
}
source_with_context("math/simple_math/test-multiply.R")
