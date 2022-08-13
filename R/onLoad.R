
.onLoad <- function(libname, pkgname){
  options(Matrix.warnDeprecatedCoerce = 2) 
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste('Development version of', pkgname, 'with options(Matrix.warnDeprecatedCoerce = 2) \n'))
}