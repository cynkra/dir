#' 'devtools' overrides
#'
#' Wrappers around 'devtools' functions that behave the same but support a flexible
#' folder structure. Not needed if you use `use_dir_package(, patch = TRUE)`,
#' which will patch the actual 'devtools' functions directly.
#'
#' @inheritParams devtools::document
#' @inheritParams devtools::check
#' @inheritParams devtools::build
#' @inheritParams devtools::test_active_file
#'
#' @return These functions return `NULL` invisibly, they're called for side effects.
#' @seealso [devtools::document()], [devtools::check()], [devtools::build()]
#' @export
#' @name devtools
document <- function(pkg = ".", roclets = NULL, quiet = FALSE) {
  files <- fetch_src_files(globals$dirs, globals$recursive)
  renamed_files <- src_path_to_r_dashed(files)
  fs::file_move(files, renamed_files)
  # try silently because document() triggers a reload so this should be
  # handled there already
  on.exit(try(fs::file_move(renamed_files, files), silent = TRUE))
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

#' @export
#' @rdname devtools
test_active_file <- function(file = find_active_file(), ...) {
  if (rstudioapi::hasFun("documentSaveAll")) {
    rstudioapi::documentSaveAll()
  }

  force(file)
  if (!fs::path_dir(file) %in% c("R", "tests/testthat")) {
    test_file <- src_path_to_test(file)
    if (fs::file_exists(test_file)) {
      file <- test_file
      testthat::test_file(test_file, package = pkgload::pkg_name(), load_package = "source", ...)
      return(invisible(NULL))
    }
  }

  devtools_test_active_file(
    file = file,
    ...
  )
}
