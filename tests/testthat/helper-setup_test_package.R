setup_test_package <- function() {
  set.seed(42) # for the praise() messages
  setHook(packageEvent("dir.example", "onLoad"), NULL, "replace")
  if (isNamespaceLoaded("dir.example")) pkgload::unload("dir.example")
  dest <- fs::path_temp("dir.example")
  globals$test_package_dir <- dest
  unlink(dest, recursive = TRUE)
  orig <- system.file("dir.example", package = "dir")
  fs::dir_create(dest)
  fs::file_temp()
  fs::dir_copy(orig, dest, TRUE)
  # remove unnecessary files and files we'll build
  fs::file_delete(fs::path(dest, c(
    # created by {dir}
    "NAMESPACE",
    ".RProfile",
    "R/sysdata.rda",
    "tests/testthat/test-nested-test-scripts.R"
  )))
  # created by {dir}
  fs::dir_delete(fs::path(dest, "man"))
  # overwrite .Rbuildignore
  code <- readLines(fs::path(orig, ".Rbuildignore"))
  code <- code[!cumsum(startsWith(code, "#"))]
  writeLines(code, fs::path(dest, ".Rbuildignore"))
  unpatch("usethis", "use_test")
  unpatch("usethis", "use_r")
  unpatch("devtools", "document")
  unpatch("devtools", "check")
  unpatch("devtools", "build")
  unpatch("devtools", "test_active_file")
  unpatch("covr", "report")
  unpatch("covr", "package_coverage")
  setwd(globals$test_package_dir)
}
