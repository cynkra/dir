#' 'devtools' overrides
#'
#' Wrappers around 'devtools' functions that behave the same but support a flexible
#' folder structure. Not needed if you use `use_dir_package(, patch = TRUE)`,
#' which will patch the actual 'devtools' functions directly.
#'
#' @inheritParams devtools::document
#' @inheritParams devtools::check
#' @inheritParams devtools::build
#'
#' @return These functions return `NULL` invisibly, they're called for side effects.
#' @seealso [devtools::document()], [devtools::check()], [devtools::build()]
#' @export
#' @name devtools
document <- function(pkg = ".", roclets = NULL, quiet = FALSE) {
  dirs <- globals$dirs
  files <- unlist(lapply(dirs, list.files, recursive = globals$recursive, full.names = TRUE, pattern = "\\.[Rr]$"))
  files <- grep("/_", files, invert = TRUE, value = TRUE)
  renamed_files <- file.path("R", gsub("/", "--", files))
  file.rename(files, renamed_files)
  on.exit(file.rename(renamed_files, files))
  devtools_document(pkg, roclets, quiet)
}

#' @export
#' @rdname devtools
check <- function(
    pkg = ".",
    document = NULL,
    build_args = NULL,
    ...,
    manual = FALSE,
    cran = TRUE,
    remote = FALSE,
    incoming = remote,
    force_suggests = FALSE,
    run_dont_test = FALSE,
    args = "--timings",
    env_vars = c(NOT_CRAN = "true"),
    quiet = FALSE,
    check_dir = NULL,
    cleanup = lifecycle::deprecated(),
    vignettes = TRUE,
    error_on = c("never", "error", "warning", "note")) {
  on.exit(document(pkg, quiet = TRUE))
  devtools_check(
    pkg,
    document,
    build_args,
    ...,
    manual = manual,
    cran = cran,
    remote = remote,
    incoming = incoming,
    force_suggests = force_suggests,
    run_dont_test = run_dont_test,
    args = args,
    env_vars = env_vars,
    quiet = quiet,
    check_dir = check_dir,
    cleanup = cleanup,
    vignettes = vignettes,
    error_on = error_on
  )
}

#' @export
#' @rdname devtools
build <- function(
    pkg = ".",
    path = NULL,
    binary = FALSE,
    vignettes = TRUE,
    manual = FALSE,
    args = NULL,
    quiet = FALSE,
    ...) {
  on.exit(document(pkg, quiet = TRUE))
  devtools_build(
    pkg = pkg,
    path = path,
    binary = binary,
    vignettes = vignettes,
    manual = manual,
    args = args,
    quiet = quiet,
  )
}
