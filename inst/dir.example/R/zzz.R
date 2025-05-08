.onLoad <- function(libname, pkgname) {
  # {dir} start
  ns <- asNamespace(pkgname)
  for (nm in names(ns)) {
    f <- ns[[nm]]
    if (is.function(f)) environment(f) <- ns
    ns[[nm]] <- f
  }
  # {dir} end
}
