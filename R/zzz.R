# `.`, `kc`, `mc`, `jt`, `mt`: bbquote() template placeholder names (the
# substitution list's names), not real globals -- R CMD check can't see
# through the quasiquotation.
utils::globalVariables(c(".", "kc", "mc", "jt", "mt"))

.onLoad <- function(libname, pkgname) {
  register_extra_blocks()
  invisible(NULL)
}
