#' 'usethis' overrides
#'
#' Wrappers around 'usethis' functions that behave the same but support a flexible
#' folder structure. Not needed if you use `use_dir_package(, patch = TRUE)`,
#' which will patch the actual 'usethis' functions directly.
#'
#' @inheritParams usethis::use_r
#' @param test_structure Either "flat" or "nested". If "flat" we'll replace
#'   "/" with "--" in paths to have flat test names. If "nested" we'll reproduce
#'   the nested structure. In tests we consider folders at the top level and
#'   sub folder of the R folder at the same level, if
#'   you name folders in R an at the top level the same there will be ambiguity..
#'
#' @return These functions return `NULL` invisibly, they're called for side effects.
#' @seealso [usethis::use_r()]
#' @export
#' @name usethis
use_r <- function(name = NULL, open = rlang::is_interactive(), test_structure = getOption("dir.test_structure", "nested")) {
  # dispatch or fail
  if (!is.null(name)) return(usethis_use_r(name, open))
  cli::cli_div(theme = usethis_theme())
  if (!rstudioapi::isAvailable()) {
    cli::cli_abort(
      "{.arg name} is absent but must be specified."
    )
  }
  path <- fs::path_rel(rstudioapi::getSourceEditorContext()$path)
  if (!fs::path_has_parent(path, "tests/testthat")) {
    return(usethis_use_r(name, open))
  }

  # Handle flat test structure
  if (test_structure == "flat") {
    if (!grepl("--", path)) {
      return(usethis_use_r(name, open))
    }
    dest_path <- sub("^tests/testthat/test-", "", gsub("--", "/", path))
  } else {
    if (fs::path_dir(path) == "tests/testthat") {
      return(usethis_use_r(name, open))
    }
    base_name <- sub("^test-", "", fs::path_file(path))
    dest_dir <- sub("^tests/testthat/", "", fs::path_dir(path))
    dest_path <- fs::path(dest_dir, base_name)
  }
  dest_path_r <- fs::path("R", dest_path)
  if (fs::file_exists(dest_path_r)) {
    dest_path <- dest_path_r
  }
  if (!fs::file_exists(dest_path)) {
    cli::cli_abort(c(
      "Can't resolve this test to an existing script",
      x = "{.path {dest_path}} doesn't exist",
      x = "{.path {dest_path_r}} doesn't exist"
    ))
  }
  edit_file(dest_path, open)
}


#' @rdname usethis
#' @export
use_test <- function(name = NULL, open = rlang::is_interactive(), test_structure = getOption("dir.test_structure", "nested")) {
  # dispatch or fail
  if (!is.null(name)) return(usethis_use_test(name, open))
  if (!rstudioapi::isAvailable()) {
    cli::cli_abort(
      "{.arg name} is absent but must be specified."
    )
  }
  path <- fs::path_rel(rstudioapi::getSourceEditorContext()$path)
  if (fs::path_dir(path) == "R" || fs::path_has_parent(path, "tests/testthat") || !fs::path_has_parent(path, ".")) {
    return(usethis_use_test(name, open))
  }

  # Handle flat test structure
  if (test_structure == "flat") {
    use_test_flat(path, open)
  } else {
    use_test_nested(path, open)
  }


}

use_test_flat <- function(path, open) {
  temp_path_r <- sub("^R/", "", path)
  temp_path_r <- src_path_to_r_dashed(temp_path_r)
  base_name_r <- fs::path_file(temp_path_r)
  with_interim_file(
    temp_path_r,{
      writeLines(readLines(path), temp_path_r)
      usethis_use_test(base_name_r, open)
    }
  )
}

use_test_nested <- function(path, open) {
  # handle nested test structure : test files
  base_name_r <- fs::path_file(path)
  base_name_tests <- paste0("test-", base_name_r)
  temp_path_r <- fs::path("R", base_name_r)
  temp_path_tests <- fs::path("tests/testthat", base_name_tests)
  dest_path <- fs::path(
    "tests/testthat",
    sub("^R/", "", fs::path_dir(path)),
    base_name_tests
  )
  if (!fs::file_exists(dest_path)) ui_bullets(c(v = "Writing {.path {dest_path}}."))
  with_interim_file(
    temp_path_tests,{
      unlink(temp_path_tests)
      if (fs::file_exists(dest_path)) writeLines(readLines(dest_path), temp_path_tests)
      suppressMessages(usethis_use_test(base_name_r, open = FALSE))
      fs::dir_create(fs::path_dir(dest_path), recurse = TRUE)
      fs::file_move(temp_path_tests, dest_path)
    }
  )

  # handle nested test structure : test file runner
  update_test_runner_file()
  edit_file(dest_path, open)
}


update_test_runner_file <- function() {
  all_test_files <- setdiff(
    list.files("tests/testthat", recursive = TRUE),
    list.files("tests/testthat")
  )
  code_fun <- c(
    'source_with_context <- function(x) {',
    '  testthat::context_start_file(sub("test-([^/]+)$", "\\\\1", x))',
    '  source(x, local = TRUE)',
    '}'
  )
  code <- c(
    code_fun,
    sprintf('source_with_context("%s")', all_test_files)
  )

  writeLines(code, "tests/testthat/test--nested-files.R")
}

# modified from usethis
edit_file <- function(path, open = rlang::is_interactive()) {
  if (!open) {
    ui_bullets(c(`_` = "Edit {.path {path}}."))
    return(invisible(path))
  }
  ui_bullets(c(`_` = "Modify {.path {path}}."))
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
    rstudioapi::navigateToFile(path)
  }
  else {
    utils::file.edit(path)
  }
  invisible(path)
}

# modified from usethis
ui_bullets <- function(text, .envir = parent.frame()) {
  cli::cli_div(theme = usethis_theme())
  cli::cli_bullets(text, .envir = .envir)
}

# vendored from usethis
usethis_theme <- function() {
  list(
    `.bullets .bullet-_` = list(`text-exdent` = 2, before = function(x) {
      paste0(
        cli::col_red(cli::symbol$checkbox_off),
        " "
      )
    }), `.bullets .bullet-i` = list(
      `text-exdent` = 2,
      before = function(x) {
        paste0(
          cli::col_yellow(cli::symbol$info),
          " "
        )
      }
    ), `.bullets .bullet-*` = list(
      `text-exdent` = 2,
      before = function(x) paste0(cli::symbol$bullet, " ")
    ),
    span.field = list(transform = single_quote_if_no_color)
  )
}

# inlined straight from usethis
single_quote_if_no_color <- function(x) quote_if_no_color(x, "'")

# inlined straight from usethis
quote_if_no_color <- function (x, quote = "'")
{
  if (cli::num_ansi_colors() > 1) {
    x
  }
  else {
    paste0(quote, x, quote)
  }
}
