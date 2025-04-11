
fetch_src_files <- function(dirs, recursive) {
  files <- unlist(lapply(dirs, list.files, recursive = recursive, full.names = TRUE, pattern = "\\.[Rr]$"))
  files <- grep("/_", files, invert = TRUE, value = TRUE)
  files
}

src_path_to_r_dashed <- function(path) {
  path <- fs::path_rel(path)
  fs::path("R", gsub("/", "--", path))
}

src_path_to_test <- function(path, test_structure = getOption("dir.test_structure", "nested")) {
  path <- fs::path_rel(path)
  path_no_r <- sub("^R/", "", path)
  if (test_structure == "nested") {
    file_name <- paste0("test-", fs::path_file(path))
    fs::path(
      "tests/testthat/",
      fs::path_dir(path_no_r),
      file_name
    )
  } else {
    file_name <- paste0("test-", gsub("/", "--", path_no_r))
    fs::path("tests/testthat/", file_name)
  }
}
