old_wd <- setup_test_package()

test_that("code of test package before modification hasn't changed", {
  expect_snapshot({
    files <- fs::dir_ls(globals$test_package_dir, recurse = TRUE, type = "file", all = TRUE)
    for (file in files) {
      writeLines(
        sprintf("\n### %s ----\n", fs::path_rel(file, globals$test_package_dir))
      )
      writeLines(readLines(file))
    }
  })
})

test_that("document and load all untouched package", {
  expect_snapshot({
    devtools::document()
    pkgload::load_all()
    ls(asNamespace("dir.example"))
    getNamespaceExports("dir.example")
    list.files("man")
  })
})

test_that("use_dir_package gives us the new objects, undocumented at this point", {
  expect_snapshot({
    use_dir_package("greetings", "math", "inst/not_build_ignored", "R/r_subfolder", patch = TRUE)
    ls(asNamespace("dir.example"))
    getNamespaceExports("dir.example")
    list.files("man")
  })
})

test_that("patched devtools::document() works", {
  expect_snapshot({
    devtools::document()
    getNamespaceExports("dir.example")
    list.files("man")
  })
})

test_that("patched devtools::test() works", {
  expect_snapshot({
    devtools::test()
  })
})


test_that("patched covr::package_coverage() works", {
  # fs::dir_copy(".", fs::path(old_wd, "../dir.example"), overwrite = TRUE)
  # this seems to work locally but here we can't get an output,
  # we still check if the R folder is in the right state

  cov <- covr::package_coverage()
  expect_true(length(cov) != 0)

  expect_snapshot({
    fs::dir_ls("R")
    names(asNamespace("dir.example"))
    e <- new.env()
    load("R/sysdata.rda", e)
    names(e)
  })
})

test_that("patched devtools::build() works", {
  #fs::dir_copy(".", fs::path(old_wd, "../dir.example"), overwrite = TRUE)
  expect_snapshot({
    # we have to make it quiet because it shows absolute paths
    # we could also edit the output or check with regex
    out <- devtools::build(quiet = TRUE)
    fs::path_rel(out)
    fs::dir_ls("R")
    names(asNamespace("dir.example"))
    e <- new.env()
    load("R/sysdata.rda", e)
    names(e)
  })
})

test_that("patched devtools::check() works", {
  # fs::dir_copy(".", fs::path(old_wd, "../dir.example"), overwrite = TRUE)
  expect_snapshot({
    # we have to make it quiet because it shows absolute paths
    # we could also edit the output or check with regex
    out <- devtools::check(quiet = TRUE)
    fs::dir_ls("R")
    names(asNamespace("dir.example"))
  })
})

setwd(old_wd)
