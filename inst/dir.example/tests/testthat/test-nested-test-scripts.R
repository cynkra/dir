files <- setdiff(
  list.files(".", pattern = "^test-.*[.][rR]$", recursive = TRUE),
  list.files(".")
)

for (file in files) {
  testthat::context_start_file(sub("test-([^/]+)$", "\\1", file))
  source(file, local = TRUE)
}
