
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dir

{dir} allows you to store your code in a nested folder structure, rather
than solely in the “R” folder.

To do so you might for instance have in your “zzz.R” file:

``` r
.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("DEVTOOLS_LOAD") == pkgname) dir::add("new_top_level_folder", "maybe_another_one")
}
```

Then use `devtools::load_all()` as you would normally, now it will load
also the code from your added folders (recursively into nested folders
by default).

`devtools::load_all()` will now also take care of the documentation so
no need to call `devtools::document()` (and if you do, it will delete
some “.Rd” files, we found no way around it).

Files and folders starting with “\_” in your added folders will be
ignored by the process.

`dir:add()` will never be called by your end users.

## Installation

Install with:

``` r
pak::pak("cynkra/dir")
```

## How does it work?

`dir::add()` will:

- Add the ‘dir’ package to your “Suggests” dependencies in the
  “DESCRIPTION” file.
- Add your added folders to “.Rbuidignore”
- Load the code from your added folders into the `sysdata.rda` file in
  the R folder.
- Load the objects into the session
- Document everything, like `devtools::document()`

## Do you have an example ?

Sure, there you go:

https//github.com/cynkra/dir.example

## History

This had been
[raised](https://stat.ethz.ch/pipermail/r-devel/2009-December/056022.html)
[on](https://stat.ethz.ch/pipermail/r-devel/2010-February/056513.html)
[r-devel](https://hypatia.math.ethz.ch/pipermail/r-devel/2023-March/082496.html)
multiple times but little interest has been shown to implement the
feature, so here we are.
