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
  backup("usethis", "use_test")
  backup("usethis", "use_r")
  backup("devtools", "document")
  backup("devtools", "build")
  backup("devtools", "check")
  backup("devtools", "test_active_file")
  backup("covr", "report")
  backup("covr", "package_coverage")
}

backup <- function(pkg, fun) {
  original <- getFromNamespace(fun, pkg)
  original_is_already_patched <- !is.null(attr(original, "original_source"))
  if (original_is_already_patched) original <- attr(original, "original_source")
  original_is_still_patched <- !is.null(attr(original, "original_source"))
  nm <- paste0(pkg, "_", fun)
  assign(nm, original, asNamespace("dir"))
}

globalVariables(c("find_active_file"))
