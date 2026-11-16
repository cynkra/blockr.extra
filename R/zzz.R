# `.`: the bbquote() template placeholder, not a real global -- R CMD check
# can't see through the quasiquotation.
utils::globalVariables(".")

.onLoad <- function(libname, pkgname) {
  register_extra_blocks()
  invisible(NULL)
}
