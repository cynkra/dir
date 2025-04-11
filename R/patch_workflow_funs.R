patch_workflow_funs <- function() {
  cli::cli_inform(c(i = "Patching 'usethis', 'devtools' and 'covr' functions"))

  # To avoid notes, justified because:
  # * It's documented clearly
  # * The user needs to opt in
  # * The package is not intended to be programmed around so no surprise propagation
  unlock_binding <- get("unlockBinding")
  lock_binding <- get("lockBinding")
  assign_in_namespace <- get("assignInNamespace")

  usethis <- asNamespace("usethis")
  unlock_binding("use_test", usethis)
  unlock_binding("use_r", usethis)
  assign_in_namespace("use_r", use_r_for_patch, usethis)
  assign_in_namespace("use_test", use_test_for_patch, usethis)
  lock_binding("use_test", usethis)
  lock_binding("use_r", usethis)

  devtools <- asNamespace("devtools")
  unlock_binding("document", devtools)
  unlock_binding("check", devtools)
  unlock_binding("build", devtools)
  unlock_binding("test_active_file", devtools)
  assign_in_namespace("document", document_for_patch, devtools)
  assign_in_namespace("check", check_for_patch, devtools)
  assign_in_namespace("build", build_for_patch, devtools)
  assign_in_namespace("test_active_file", test_active_file_for_patch, devtools)
  lock_binding("document", devtools)
  lock_binding("check", devtools)
  lock_binding("build", devtools)
  lock_binding("test_active_file", devtools)

  covr <- asNamespace("covr")
  unlock_binding("report", covr)
  unlock_binding("package_coverage", covr)
  assign_in_namespace("report", report_for_patch, covr)
  assign_in_namespace("package_coverage", package_coverage_for_patch, covr)
  lock_binding("report", covr)
  lock_binding("package_coverage", covr)
}

use_r_for_patch <- function(name = NULL, open = rlang::is_interactive()) {
  cli::cli_inform(cli::col_grey("patched to `dir::use_r()`"))
  dir::use_r(name, open)
}

use_test_for_patch <- function(name = NULL, open = rlang::is_interactive()) {
  cli::cli_inform(cli::col_grey("patched to `dir::use_test()`"))
  dir::use_test(name, open)
}

document_for_patch <- function(pkg = ".", roclets = NULL, quiet = FALSE) {
  cli::cli_inform(cli::col_grey("patched to `dir::document()`"))
  dir::document(pkg, roclets, quiet)
}

check_for_patch <- function(
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
  cli::cli_inform(cli::col_grey("patched to `dir::check()`"))
  dir::check(
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

build_for_patch <- function(
    pkg = ".",
    path = NULL,
    binary = FALSE,
    vignettes = TRUE,
    manual = FALSE,
    args = NULL,
    quiet = FALSE,
    ...) {
  cli::cli_inform(cli::col_grey("patched to `dir::build()`"))
  dir::build(
    pkg = pkg,
    path = path,
    binary = binary,
    vignettes = vignettes,
    manual = manual,
    args = args,
    quiet = quiet,
  )
}

report_for_patch <- function(
    x = dir::package_coverage(),
    file = file.path(tempdir(), paste0(attr(x, "package")$package %||% "coverage", "-report.html")),
    browse = interactive()) {
  cli::cli_inform(cli::col_grey("patched to `dir::report()`"))
  dir::report(x, file, browse)
}

package_coverage_for_patch <- function(path = ".", type = c("tests", "vignettes", "examples",
                                                            "all", "none"), combine_types = TRUE, relative_path = TRUE,
                                       quiet = TRUE, clean = TRUE, line_exclusions = NULL, function_exclusions = NULL,
                                       code = character(), install_path = normalizePath(tempfile("R_LIBS"), mustWork = FALSE), ...,
                                       exclusions, pre_clean = TRUE) {
  cli::cli_inform(cli::col_grey("patched to `dir::package_coverage()`"))
  if (missing(type)) {
    type <- "tests"
  }
  dir::package_coverage(
    path = path,
    type = type,
    combine_types = combine_types,
    relative_path = relative_path,
    quiet = quiet,
    clean = clean,
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions,
    code = code,
    install_path = install_path,
    ...,
    exclusions = exclusions,
    pre_clean = pre_clean
  )
}

test_active_file_for_patch <- function(file = find_active_file(), ...) {
  cli::cli_inform(cli::col_grey("patched to `dir::test_active_file()`"))
  dir::test_active_file(
    file = file,
    ...
  )
}
