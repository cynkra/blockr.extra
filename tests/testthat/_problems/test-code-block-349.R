# Extracted from test-code-block.R:349

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "blockr.extra", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
testServer <- shiny::testServer
specs_for <- function(script, data = iris) {
  cb_specs(cb_parse(script), data)
}
expr_for <- function(script, values = list(), data = iris) {
  p <- cb_parse(script)
  cb_expr(p, cb_specs(p, data), values)
}

# test -------------------------------------------------------------------------
script <- paste(
    'vars <- c("Species", "Petal.Width")',
    'vars <- intersect(vars, names(data))',
    'data[, vars, drop = FALSE]',
    sep = "\n"
  )
p <- cb_parse(script)
expect_equal(attr(cb_demoted(p), "reason"), c("redeclared", "", ""))
s <- cb_specs(p, iris)
