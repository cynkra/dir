# similar to withr::with_file, but restores original file if exists
with_interim_file <- function(file, code) {
  file <- as.list(file)
  file_nms <- rlang::names2(file)
  unnamed <- file_nms == ""
  file_nms[unnamed] <- unlist(file[unnamed])
  file[unnamed] <- replicate(sum(unnamed), character())
  # backup existing files, restore on exit
  existing_files <- file_nms[file.exists(file_nms)]
  if (length(existing_files)) {
    tmp_files <- replicate(length(existing_files), tempfile())
    file.rename(existing_files, tmp_files)
    on.exit(file.rename(tmp_files, existing_files))
  }
  on.exit(unlink(file_nms, recursive = TRUE), add = TRUE, after = FALSE)
  for (i in seq_along(file_nms)) writeLines(file[[i]], file_nms[[i]])
  force(code)
}

