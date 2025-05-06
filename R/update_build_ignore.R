update_build_ignore <- function(dirs) {
  # exclude inst and folders under inst
  dirs <- setdiff(dirs[!fs::path_has_parent(dirs, "inst")], "inst")

  # remove previous lines
  build_ignore <-
    if (file.exists(".Rbuildignore")) readLines(".Rbuildignore") else character()

  start_remove <- which(build_ignore == c("# dir::add() start"))
  end_remove <- which(build_ignore == c("# dir::add() end"))
  if (length(start_remove)) {
    build_ignore <- build_ignore[-(start_remove:end_remove)]
  }
  # add new lines
  build_ignore <- c(
    build_ignore,
    "# dir::add() start",
    paste0("^", dirs, "$"),
    "# dir::add() end"
  )
  # save
  writeLines(build_ignore, ".Rbuildignore")
}
