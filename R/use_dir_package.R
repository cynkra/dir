#' Setup the package to use 'dir'
#'
#' This is the only function you need to use 'dir'. It sets up your local
#' ".Rprofile" so you can use nested folders in your project. You can tweak the
#' ".RProfile" manually if needed or re-rerun the function to override the setup code.
#'
#' @param ... Folders to add. You might provide strings, vectors or lists, they will be flattened into a vector.
#' @param recursive Boolean. Whether to add folders under the added folders recursively.
#' @param patch Boolean. Whether to patch some 'usethis' and 'devtools' functions so they
#'   work with 'dir'. Recommended but since it is invasive it defaults to `FALSE`.
#'   Forwarded to `dir::add()`
#' @param add_overrides Boolean. Whether to add overrides for 'usethis' and 'devtools'
#'   functions in a `dir-overrides` environment on the search path. These can be
#'   adjusted to your taste in the ".Rprofile".
#
#' @return Returns `NULL` invisibly, called for side effects.
#' @export
use_dir_package <- function(..., recursive = TRUE, patch = FALSE, add_overrides = !patch) {
  # build hook code ============================================================
  if (!...length()) {
    rlang::abort("Please provide at least one folder")
  }

  # build hook code ============================================================
  pkg <- pkgload::pkg_name()
  hook <- bquote(
    setHook(packageEvent(.(pkg), "onLoad"), function(...) .(substitute(dir::add(...))), "replace")
  )
  args <- list(recursive = recursive, patch = patch, add_overrides = add_overrides)
  if (recursive) args$recursive <- NULL
  if (!patch) args$patch <- NULL
  if (add_overrides == !patch) args$add_overrides <- NULL
  if (length(args)) hook[[3]][[3]] <- as.call(c(as.list(hook[[3]][[3]]), args))
  hook_code <- deparse1(hook)
  code <- c(
    '### start of {dir} setup ###',
    '# See `?dir::use_dir_package`',
    'options(dir.test_structure = "nested") # "nested" or "flat", see `?dir::use_r()`',
    hook_code
  )
  code_overrides <- if (add_overrides) {
    c(
      'attach(name = "dir-overrides", NULL)',
      'makeActiveBinding("use_test", function() dir::use_test, as.environment("dir-overrides"))',
      'makeActiveBinding("use_r", function() dir::use_r, as.environment("dir-overrides"))'
      # 'makeActiveBinding("document", function() dir::document, as.environment("dir-overrides"))'
    )
  }
  code <- c(
    code,
    code_overrides,
    '### end of {dir} setup ###'
  )

  # Edit .RProfile =============================================================

  cli::cli_inform(c(i = "Editing {.path .RProfile}"))
  if (!file.exists(".Rprofile")) {
    rprofile <- c('source("~/.Rprofile") # source user level R profile\n\n', code, "")
  } else {
    rprofile <- readLines(".Rprofile")
    start <- which(rprofile == '### start of {dir} setup ###')
    end <- which(rprofile == '### end of {dir} setup ###')
    if (length(start)) {
      rprofile <- rprofile[-(start:end)]
    }
    # trim trailing lines
    rprofile <- rprofile[rev(cumprod(rev(rprofile == "")) == 0)]
    rprofile <- c(rprofile, "", code, "")
  }
  writeLines(rprofile, ".RProfile")
  if (interactive()) suppressMessages(usethis::edit_r_profile("project"))

  # Setup hook a first time ====================================================
  eval.parent(parse(text = code))

  # Load =======================================================================
  pkgload::load_all()

  # Edit .onLoad ===============================================================
  .onLoad <- asNamespace(pkg)$.onLoad
  if (!is.null(.onLoad)) {
    srcref <- attr(.onLoad, "srcref")
    on_load_code <- capture.output(.onLoad)
    on_load_code <- on_load_code[-length(on_load_code)] # remove namespace line
    on_load_file <- trimws(capture.output(attr(srcref, "srcfile")))
    lines <- readLines(on_load_file)
    start_pos <- which(startsWith(lines, ".onLoad"))
    lines <- lines[-((start_pos - 1) + 1:length(on_load_code))]
    on_load_code[[1]] <- ".onLoad <- function(libname, pkgname) {"
    on_load_code <- on_load_code[cumsum(startsWith(on_load_code, "# {dir}")) != 1]
    on_load_code <- on_load_code[-length(on_load_code)] # remove closing "}"
    on_load_code <- c(
      on_load_code,
      "  # {dir} start",
      "  ns <- asNamespace(pkgname)",
      "  for (nm in names(ns)) {",
      "    f <- ns[[nm]]",
      "    if (is.function(f)) environment(f) <- ns",
      "    ns[[nm]] <- f",
      "  }",
      "  # {dir} end",
      "}"
    )
    lines <- append(lines, on_load_code, after = start_pos - 1)
    writeLines(lines, on_load_file)
  } else {
    on_load_code <- c(
      ".onLoad <- function(libname, pkgname) {",
      "  # {dir} start",
      "  ns <- asNamespace(pkgname)",
      "  for (nm in names(ns)) {",
      "    f <- ns[[nm]]",
      "    if (is.function(f)) environment(f) <- ns",
      "    ns[[nm]] <- f",
      "  }",
      "  # {dir} end",
      "}"
    )
    writeLines(on_load_code, "R/zzz.R")
  }

  use_dir_tests()

  cli::cli_inform(c(">" = "You're good to go!"))
}


use_dir_tests <- function() {
  suppressMessages(usethis::use_testthat())
  file <- system.file("dir.example/tests/testthat/test-nested-test-scripts.R", package = "dir")
  fs::file_copy(file, "tests/testthat/test-nested-test-scripts.R")
}
