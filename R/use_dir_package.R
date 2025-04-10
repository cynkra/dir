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
  withr::local_dir(rprojroot::find_root(rprojroot::is_r_package))
  pkg <- pkgload::pkg_name()

  hook <- bquote(
    setHook(packageEvent(.(pkg), "onLoad"), function(...) .(substitute(dir::add(...))), "replace")
  )
  args <- list(recursive = recursive, document, patch = patch, add_overrides = add_overrides)
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
  suppressMessages(usethis::edit_r_profile("project"))
  eval.parent(parse(text = code))
  pkgload::load_all()
  cli::cli_inform(c(">" = "You're good to go!"))
}
