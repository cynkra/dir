globals <- new.env()
globals$dirs <- NULL
globals$recursive <- NULL

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
  usethis_use_test <<- usethis::use_test
  usethis_use_r <<- usethis::use_r

  devtools_document <<- devtools::document
  devtools_build <<- devtools::build
  devtools_check <<- devtools::check
  devtools_test_active_file <<- devtools::test_active_file

  covr_report <<- covr::report
  covr_package_coverage <<- covr::package_coverage
}

