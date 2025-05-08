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
  # sometimes we need to move scripts temporarily to R to call an external
  # function and this function triggers a reload, in those cases we need
  # to return early or this will corrupt sysdata.rda (examples: devtools::build)
  if (globals$flat_state) return(invisible(NULL))

  if (globals$skip) {
    # patch workflow functions ===================================================
    if (patch) patch_workflow_funs()
    globals$skip <- FALSE
    return(invisible(NULL))
  }

  # cut short if called by tools pkg ===========================================
  # covr triggers installations, which in turn trigger some reloads, but we don't
  # want to rename our files when it happens
  # this is not debuggable with browser() or messages when it happens
  called_from_tools <- identical(topenv(sys.frame(1)), asNamespace("tools"))
  if (called_from_tools) return(invisible(NULL))

  cli::cli_inform(c(i = "Loading external and nested folders"))
  # setup ======================================================================
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

  # update .Rbuildignore  ======================================================
  update_build_ignore(dirs)

  # update sysdata.rda =========================================================
  e <- new.env()
  for (file in files) source(file, local = e)

  # set environment of functions and env formulas to ns
  objs <- eapply(e, function(x) {
    if (is.function(x) || rlang::is_formula("formula")) {
      environment(x) <- .GlobalEnv
    }
    x
  })
  save(list = names(e), file = "R/sysdata.rda", envir = as.environment(objs), compress = "bzip2", version = 3, ascii = FALSE)

  globals$skip <- TRUE

  devtools::load_all(".", quiet = TRUE)
}
