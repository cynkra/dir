globals <- new.env()
globals$dirs <- NULL
globals$recursive <- NULL

usethis_use_test <- NULL
usethis_use_r <- NULL
devtools_document <- NULL
devtools_build <- NULL
devtools_check <- NULL

.onLoad <- function(...) {
  options(dir.test_structure = getOption("dir.test_structure", "nested"))
  usethis_use_test <<- usethis::use_test
  usethis_use_r <<- usethis::use_r
  devtools_document <<- devtools::document
  devtools_build <<- devtools::build
  devtools_check <<- devtools::check
}
