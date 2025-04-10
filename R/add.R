
#' Add Folders to a R package
#'
#' @description
#'
#' `add()` allows you to store your code in a nested folder structure, rather than
#' solely in the "R" folder. To do so:
#'
#' * Have a call to `dir::add()` in your `.onLoad()` definition as shown
#'  in the examples at the bottom of this page.
#' * Call `devtools::load_all()` in your workflow, and never `devtools::document()`.
#' Indeed `load_all()` will document everything through `dir::add()`, and
#' `devtools::document()` would drop some ".Rd" files.
#'
#' Files and folders starting with "_" in your added folders will be ignored by the process.
#'
#' @section How does it work?:
#'
#' `dir::add()` will:
#'
#' * Add the 'dir' package to your "Suggests" dependencies in the "DESCRIPTION" file.
#' * Add your added folders to ".Rbuidignore"
#' * Load the code from your added folders into the `sysdata.rda` file in the R folder.
#' * Load the objects into the session
#' * Document everything, like `devtools::document()`
#'
#' @param ... Folders to add. You might provide strings, vectors or lists, they
#'   will be flattened into a vector.
#' @param .recursive Boolean. Whether to add folders under the added folders recursively.
#'
#' @export
#' @examples
#' \dontrun{
#' .onLoad <- function(libname, pkgname) {
#'   # the condition makes sure this is run only during development
#'   if (Sys.getenv("DEVTOOLS_LOAD") == pkgname) dir::add("new_top_level_folder", "maybe_another_one")
#' }
#' }
add <- function(..., .recursive = TRUE) {
  # setup ======================================================================
  usethis::use_package("dir", "Suggests")
  withr::local_dir(here::here())
  dirs <- sort(unlist(list(...)))
  dirs_exist <- sapply(dirs, dir.exists)
  if (!all(dirs_exist)) {
    msg <- "Additional dirs must exist"
    info <- sprintf("Not found: %s", toString(shQuote(dirs[!dirs_exist])))
    rlang::abort(c(msg, x = info))
  }
  files <- unlist(lapply(dirs, list.files, recursive = .recursive, full.names = TRUE, pattern = "\\.[Rr]$"))
  files <- grep("/_", files, invert = TRUE, value = TRUE)

  # update .Rbuildignore  ======================================================
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

  # update sysdata.rda =========================================================
  e <- new.env()
  for (file in files) source(file, local = e)
  ns <- asNamespace(pkgload::pkg_name())

  # set environment of functions and env formulas to ns
  objs <- eapply(e, function(x) {
    if (is.function(x) || rlang::is_formula("formula")) {
      environment(x) <- ns
    }
    x
  })
  save(list = names(e), file = "R/sysdata.rda", envir = as.environment(objs), compress = "bzip2", version = 3, ascii = FALSE)

  # update the namespace with these values =====================================
  # sysdata.rda was already read when we call add() so we move them to the namespace manually
  rlang::env_unlock(ns)
  for (nm in intersect(names(objs), names(ns))) {
      unlockBinding(nm, ns)
  }
  list2env(objs, ns)
  for (nm in names(objs)) {
    lockBinding(nm, ns)
  }
  rlang::env_lock(ns)

  # document ===================================================================
  renamed_files <- file.path("R", gsub("/", "--", files))
  file.rename(files, renamed_files)
  on.exit(file.rename(renamed_files, files))
  # load_code is set so the code is not reloaded after reoxygize()
  roxygen2::roxygenize(".", load_code = function(x) ns)
  pkgload::dev_topic_index_reset(".")
}
