# code of test package before modification hasn't changed

    Code
      files <- fs::dir_ls(globals$test_package_dir, recurse = TRUE, type = "file",
      all = TRUE)
      for (file in files) {
        writeLines(sprintf("\n### %s ----\n", fs::path_rel(file, globals$
          test_package_dir)))
        writeLines(readLines(file))
      }
    Output
      
      ### .Rbuildignore ----
      
      ^LICENSE\.md$
      ^.RProfile$
      ^README\.Rmd$
      ^dir\.example\.Rproj$
      
      ### DESCRIPTION ----
      
      Package: dir.example
      Title: Showcases how the 'dir' package works
      Version: 1.1.0.9000
      Authors@R: c(
          person("Antoine", "Fabri", , "antoine.fabri@gmail.com", role = c("aut", "cre")),
          person("cynkra GmbH", , , "mail@cynkra.com", role = "fnd",
                 comment = c(ROR = "0335t7e62"))
        )
      Description: Showcases how the 'dir' package works.
      License: MIT + file LICENSE
      Encoding: UTF-8
      Roxygen: list(markdown = TRUE)
      RoxygenNote: 7.3.2.9000
      Depends:
          R (>= 3.5)
      Suggests: 
          testthat (>= 3.0.0)
      Config/testthat/edition: 3
      URL: https://github.com/cynkra/dir.example
      BugReports: https://github.com/cynkra/dir.example/issues
      
      ### LICENSE ----
      
      YEAR: 2025
      COPYRIGHT HOLDER: dir.example authors
      
      ### LICENSE.md ----
      
      # MIT License
      
      Copyright (c) 2025 dir.example authors
      
      Permission is hereby granted, free of charge, to any person obtaining a copy
      of this software and associated documentation files (the "Software"), to deal
      in the Software without restriction, including without limitation the rights
      to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      copies of the Software, and to permit persons to whom the Software is
      furnished to do so, subject to the following conditions:
      
      The above copyright notice and this permission notice shall be included in all
      copies or substantial portions of the Software.
      
      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      SOFTWARE.
      
      ### R/r_subfolder/reverse.R ----
      
      #' reverse a string
      #'
      #' @param x string
      #'
      #' @export
      reverse <- function(x) {
        paste(rev(strsplit(x, "")[[1]]), collapse = "")
      }
      
      ### R/utils.R ----
      
      #' shout
      #'
      #' @param x string
      #' @export
      shout <- function(x) {
        message(toupper(x))
      }
      
      ### README.Rmd ----
      
      ---
      output: github_document
      ---
      
      <!-- README.md is generated from README.Rmd. Please edit that file -->
      
      ```{r, include = FALSE}
      knitr::opts_chunk$set(
        collapse = TRUE,
        comment = "#>",
        fig.path = "man/figures/README-",
        out.width = "100%"
      )
      ```
      
      # dir.example
      
      'dir.example' shows how the ['dir'](https://github.com/cynkra/dir) package works.
      
      Look at the code and you'll see we have some code in the R folder but also in 2
      other folders named "greetings" and "math", where we have another nested folder.
      
      To set up this project to use dir we used the following command:
      
      ```r
      dir::use_dir_package("greetings", "math", "inst/not_build_ignored", "R/r_subfolder", patch = TRUE)
      ```
      
      We see that :
      
      * The R files are properly sourced from everywhere
      * The doc is properly knitted
      * The file "math/_sandbox.R" is ignored due to the "_" prefix (files starting with "." are ignored too)
      * The script "inst/not_build_ignored/answer.R" is not build ignored since (we don't build ignore code stored under "inst")
      * `devtools::check()` passes
      * `devtools::test()` passes
      * The package doesn't depend on {dir} (not even suggested)
      
      ## Installation
      
      Install with:
      
      ```r
      pak::pak("cynkra/dir/inst/dir.example")
      ```
      
      ### README.md ----
      
      
      <!-- README.md is generated from README.Rmd. Please edit that file -->
      
      # dir.example
      
      ‘dir.example’ shows how the [‘dir’](https://github.com/cynkra/dir)
      package works.
      
      Look at the code and you’ll see we have some code in the R folder but
      also in 2 other folders named “greetings” and “math”, where we have
      another nested folder.
      
      To set up this project to use dir we used the following command:
      
      ``` r
      dir::use_dir_package("greetings", "math", "inst/not_build_ignored", "R/r_subfolder", patch = TRUE)
      ```
      
      We see that :
      
      - The R files are properly sourced from everywhere
      - The doc is properly knitted
      - The file “math/*sandbox.R” is ignored due to the ”*” prefix (files
        starting with “.” are ignored too)
      - The script “inst/not_build_ignored/answer.R” is not build ignored
        since (we don’t build ignore code stored under “inst”)
      - `devtools::check()` passes
      - `devtools::test()` passes
      - The package doesn’t depend on {dir} (not even suggested)
      
      ## Installation
      
      Install with:
      
      ``` r
      pak::pak("cynkra/dir/inst/dir.example")
      ```
      
      ### greetings/hello.R ----
      
      #' Shout Hello
      #'
      #' @export
      hello <- function() {
        shout("hello!")
      }
      
      ### inst/not_build_ignored/answer.R ----
      
      answer <- function(question) {
        42
      }
      
      ### math/_sandbox.R ----
      
      just_playing_around <- 42
      
      stop("oops this fails!")
      
      ### math/simple_math/multiply.R ----
      
      #' Multiply two numbers
      #'
      #' @param x numeric
      #' @param y numeric
      #'
      #' @export
      multiply <- function(x, y) {
        x * y
      }
      
      ### tests/testthat/math/simple_math/test-multiply.R ----
      
      test_that("multiplication works", {
        expect_equal(2 * 2, 4)
      })
      
      ### tests/testthat.R ----
      
      # This file is part of the standard setup for testthat.
      # It is recommended that you do not modify it.
      #
      # Where should you do additional test configuration?
      # Learn more about the roles of various files in:
      # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      # * https://testthat.r-lib.org/articles/special-files.html
      
      library(testthat)
      library(dir.example)
      
      test_check("dir.example")

# document and load all untouched package

    Code
      devtools::document()
    Message
      i Updating dir.example documentation
      i Loading dir.example
      Writing 'NAMESPACE'
      Writing 'shout.Rd'
    Code
      pkgload::load_all()
    Message
      i Loading dir.example
    Code
      ls(asNamespace("dir.example"))
    Output
      [1] "shout"
    Code
      getNamespaceExports("dir.example")
    Output
      [1] "shout"
    Code
      list.files("man")
    Output
      [1] "shout.Rd"

# use_dir_package gives us the new objects, undocumented at this point

    Code
      use_dir_package("greetings", "math", "inst/not_build_ignored", patch = TRUE)
    Message
      i Editing '.RProfile'
      i Loading dir.example
      i Loading external and nested folders
      i Patching 'usethis', 'devtools' and 'covr' functions
      > You're good to go!
    Code
      ls(asNamespace("dir.example"))
    Output
      [1] "answer"   "hello"    "multiply" "reverse"  "shout"   
    Code
      getNamespaceExports("dir.example")
    Output
      [1] "shout"
    Code
      list.files("man")
    Output
      [1] "shout.Rd"

# patched devtools::document() works

    Code
      devtools::document()
    Message
      patched to `dir::document()`
      i Updating dir.example documentation
      i Loading dir.example
      Writing 'NAMESPACE'
      Writing 'reverse.Rd'
      Writing 'hello.Rd'
      Writing 'multiply.Rd'
    Code
      getNamespaceExports("dir.example")
    Output
      [1] "shout"
    Code
      list.files("man")
    Output
      [1] "hello.Rd"    "multiply.Rd" "reverse.Rd"  "shout.Rd"   

# patched devtools::build() works

    Code
      out <- devtools::build(quiet = TRUE)
    Message
      patched to `dir::build()`
      i Loading dir.example
    Code
      fs::path_rel(out)
    Output
      ../dir.example_1.1.0.9000.tar.gz
    Code
      fs::dir_ls("R")
    Output
      R/r_subfolder R/sysdata.rda R/utils.R     R/zzz.R       
    Code
      names(asNamespace("dir.example"))
    Output
       [1] ".onLoad"              ".__DEVTOOLS__"        ".packageName"        
       [4] "reverse"              "shout"                "answer"              
       [7] ".__NAMESPACE__."      "hello"                ".__S3MethodsTable__."
      [10] "multiply"            
    Code
      e <- new.env()
      load("R/sysdata.rda", e)
      names(e)
    Output
      [1] "reverse"  "answer"   "hello"    "multiply"

# patched devtools::check() works

    Code
      out <- devtools::check(quiet = TRUE)
    Message
      patched to `dir::check()`
      patched to `dir::document()`
      i Loading dir.example
      i Loading dir.example
    Code
      fs::dir_ls("R")
    Output
      R/r_subfolder R/sysdata.rda R/utils.R     R/zzz.R       
    Code
      names(asNamespace("dir.example"))
    Output
       [1] ".onLoad"              ".__DEVTOOLS__"        ".packageName"        
       [4] "reverse"              "shout"                "answer"              
       [7] ".__NAMESPACE__."      "hello"                ".__S3MethodsTable__."
      [10] "multiply"            

