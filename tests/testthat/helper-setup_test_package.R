setup_test_package <- function() {
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
    "tests/testthat/helper-nested-files.R"
  )))
  # created by {dir}
  fs::dir_delete(fs::path(dest, "man"))
  # overwrite .Rbuildignore
  code <- readLines(fs::path(orig, ".Rbuildignore"))
  code <- code[!cumsum(startsWith(code, "#"))]
  writeLines(code, fs::path(dest, ".Rbuildignore"))
  setwd(globals$test_package_dir)
}
