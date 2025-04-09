
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dir <img src="man/figures/logo.png" align="right" height="120" alt="" />

{dir} allows you to store your code in a nested folder structure, rather
than solely in the “R” folder.

Set it up with one line of boiler plate code as explained in the setup
section, from there on calling `devtools::load_all()` will:

- Load the code from the R folder AND your added folders (recursively
  into nested folders by default)
- `document()` everything, so no need to call `devtools::document()` (In
  fact calling the latter will undocument some objects, an unfortunate
  side effect of the magic we use)

Files and folders starting with “\_” or “.” in your added folders will
be ignored by the process.

{dir} won’t be needed by the end users of your package, and yes your
package is CRAN compliant.

## Installation

Install with:

``` r
pak::pak("cynkra/dir")
```

## Setup

To set this up, you need to trigger a call to `dir::add()` for
developers only. There are 2 ways to do it, through `.onLoad()` or
through a local “.RProfile”.

To do so you might for instance have in your “zzz.R” file:

``` r
.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("DEVTOOLS_LOAD") == pkgname) dir::add("new_top_level_folder", "maybe_another_one")
}
```

Or have this line in your local “.RProfile”
(`usethis::edit_r_profile("project")`)

``` r
setHook(packageEvent("your.package.name", "onLoad"), function(...) dir::add("new_top_level_folder", "maybe_another_one"))
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

Sure, [there you go!](https://github.com/cynkra/dir.example)

## Minor annoyances

- `devtools::document()` won’t work as intended, it will mess with your
  doc and NAMESPACE file, but you don’t need it, use only `load_all()`.
  If you call it by mistake `load_all()` will repair the mess.
- `devtools::check()` runs `document()` at the end of the checks, that
  messes with the doc and the NAMESPACE file as well. The check itself
  is not affected though, here as well `load_all()` will clean up the
  mess.

## History

This had been
[raised](https://stat.ethz.ch/pipermail/r-devel/2009-December/056022.html)
[on](https://stat.ethz.ch/pipermail/r-devel/2010-February/056513.html)
[r-devel](https://hypatia.math.ethz.ch/pipermail/r-devel/2023-March/082496.html)
multiple times but little interest has been shown to implement the
feature, so here we are.
