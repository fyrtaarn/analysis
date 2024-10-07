loadpkg <- function(pkg){
  pkgx <- pkg[!pkg %in% installed.packages()[, "Package"]]
  if (length(pkgx))
    install.packages(pkgx)
  invisible(lapply(pkg, library, character.only = TRUE))
}
