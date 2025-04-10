#' Add Folders to a R package
#'
#' @description
#'
#' `add()` makes sure scripts stored outside of the "R" folder are added to `.Rbuildignore`
#' and turns these scripts into objects stored in `R/sysdata.rda`.
#' `use_dir_package()` will place a call to `dir::add()` in your ".RProfile".
#' You shouldn't need to call it manually.
#'
#' @inheritParams use_dir_package
#'
#' @export
add <- function(..., recursive = TRUE, patch = FALSE) {
  # cut short if called by tools pkg ===========================================
  # covr triggers installations, which in turn trigger some reloads, but we don't
  # want to rename our files when it happens
  # this is not debuggable with browser() or messages when it happens
  called_from_tools <- identical(topenv(sys.frame(1)), asNamespace("tools"))
  if (called_from_tools) return(invisible(NULL))

  cli::cli_inform(c(i = "Loading external and nested folders"))
  # setup ======================================================================
  withr::local_dir(rprojroot::find_root(rprojroot::is_r_package))
  dirs <- sort(unlist(list(...)))
  globals$dirs <- dirs
  globals$recursive <- recursive
  dirs_exist <- sapply(dirs, dir.exists)
  if (!all(dirs_exist)) {
    msg <- "Additional dirs must exist"
    info <- sprintf("Not found: %s", toString(shQuote(dirs[!dirs_exist])))
    rlang::abort(c(msg, x = info))
  }
  files <- unlist(lapply(dirs, list.files, recursive = recursive, full.names = TRUE, pattern = "\\.[Rr]$"))
  files <- grep("/_", files, invert = TRUE, value = TRUE)

  # happens when reloading automatically through document
  # also to be safe and recover from bugs
  directory_already_flattened <-
    !length(files) && any(grepl("--", list.files("R", pattern = "[.][rR]$")))
  if (directory_already_flattened) {
    renamed_files <- grep("--", fs::dir_ls("R", regexp = "[.][rR]$"), value = TRUE)
    files <- gsub("--", "/", fs::path_rel(renamed_files, "R"))
    fs::file_move(renamed_files, files)
  }

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
  # this is safe, that's applied on the package under development
  unlock_binding <- get("unlockBinding")
  lock_binding <- get("lockBinding")
  rlang::env_unlock(ns)
  for (nm in intersect(names(objs), names(ns))) {
    unlock_binding(nm, ns)
  }
  list2env(objs, ns)
  for (nm in names(objs)) {
    lock_binding(nm, ns)
  }
  rlang::env_lock(ns)

  # patch workflow functions ===================================================
  if (patch) patch_workflow_funs()

  # document ===================================================================
  # renamed_files <- file.path("R", gsub("/", "--", files))
  # file.rename(files, renamed_files)
  # on.exit(file.rename(renamed_files, files))
  # load_code is set so the code is not reloaded after reoxygenize()

  # cli::cli_inform(c(i = "Updating documentation"))
  # roxygen2::roxygenize(".", load_code = function(x) ns)

  # pkgload::dev_topic_index_reset(".")
}
