.onLoad <- function(libname, pkgname) {
  assign('mice.impute.logreg.p',mice.impute.logreg.p,envir = .GlobalEnv)
}

.onAttach <- function(libname, pkgname) {
  assign('mice.impute.logreg.p',mice.impute.logreg.p,envir = .GlobalEnv)
}
