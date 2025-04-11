#' 'covr' overrides
#'
#' Wrappers around 'covr' functions that behave the same but support a flexible
#' folder structure. Not needed if you use `use_dir_package(, patch = TRUE)`,
#' which will patch the actual 'covr' functions directly.
#'
#' @inheritParams covr::report
#'
#' @return These functions return `NULL` invisibly, they're called for side effects.
#' @seealso [devtools::document()], [devtools::check()], [devtools::build()]
#' @export
#' @name covr
report <- function(
    x = dir::package_coverage(),
    file = file.path(tempdir(), paste0(attr(x, "package")$package %||% "coverage", "-report.html")),
    browse = interactive()) {
  withr::local_options(covr.filter_non_package = FALSE)
  dirs <- globals$dirs
  files <- unlist(lapply(dirs, list.files, recursive = globals$recursive, full.names = TRUE, pattern = "\\.[Rr]$"))
  files <- grep("/_", files, invert = TRUE, value = TRUE)
  renamed_files <- file.path("R", gsub("/", "--", files))
  fs::file_move(files, renamed_files)
  force(x)
  # try silently because document() triggers a reload so this should be
  # handled there already
  on.exit(try(fs::file_move(renamed_files, files), silent = TRUE))
  covr_report(x, file, browse)
}

#' @export
#' @rdname covr
package_coverage <- function(path = ".", type = c("tests", "vignettes", "examples",
                               "all", "none"), combine_types = TRUE, relative_path = TRUE,
          quiet = TRUE, clean = TRUE, line_exclusions = NULL, function_exclusions = NULL,
          code = character(), install_path = normalizePath(tempfile("R_LIBS"), mustWork = FALSE), ...,
          exclusions, pre_clean = TRUE) {
  withr::local_options(covr.filter_non_package = FALSE)
  if (missing(type)) {
    type <- "tests"
  }
  dirs <- globals$dirs
  files <- unlist(lapply(dirs, list.files, recursive = globals$recursive, full.names = TRUE, pattern = "\\.[Rr]$"))
  files <- grep("/_", files, invert = TRUE, value = TRUE)
  renamed_files <- file.path("R", gsub("/", "--", files))
  fs::file_move(files, renamed_files)

  # try silently because document() triggers a reload so this should be
  # handled there already
  on.exit({try(fs::file_move(renamed_files, files), silent = TRUE)})

  # note: package_coverage takes paths from src_ref so we don't have any
  # replacement to do on the output
  covr_package_coverage(
    path = path,
    type = type,
    combine_types = combine_types,
    relative_path = relative_path,
    quiet = quiet,
    clean = clean,
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions,
    install_path,
    code = code,
    install_path = install_path,
    ...,
    exclusions = exclusions,
    pre_clean = pre_clean
  )
}


