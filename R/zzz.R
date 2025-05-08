globals <- new.env()
globals$dirs <- NULL
globals$recursive <- NULL
globals$skip <- FALSE
globals$flat_state <- FALSE

usethis_use_test <- NULL
usethis_use_r <- NULL

devtools_document <- NULL
devtools_build <- NULL
devtools_check <- NULL
devtools_test_active_file <- NULL

covr_report <- NULL
covr_package_coverage <- NULL

.onLoad <- function(...) {
  options(dir.test_structure = getOption("dir.test_structure", "nested"))
  # patch only if not patched yet, to avoid circularity
  if (is.null(usethis_use_test)) usethis_use_test <<- usethis::use_test

  if (is.null(usethis_use_r)) usethis_use_r <<- usethis::use_r

  if (is.null(devtools_document)) devtools_document <<- devtools::document
  if (is.null(devtools_build)) devtools_build <<- devtools::build
  if (is.null(devtools_check)) devtools_check <<- devtools::check
  if (is.null(devtools_test_active_file)) devtools_test_active_file <<- devtools::test_active_file

  if (is.null(covr_report)) covr_report <<- covr::report
  if (is.null(covr_package_coverage)) covr_package_coverage <<- covr::package_coverage
}

globalVariables(c("find_active_file"))
