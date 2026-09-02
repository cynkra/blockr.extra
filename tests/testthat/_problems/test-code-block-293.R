# Extracted from test-code-block.R:293

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
    'vars <- intersect(names(data), c("Species", "Petal.Width", "NOPE"))',
    'data[, vars, drop = FALSE]',
    sep = "\n"
  )
s <- specs_for(script)
expect_length(s, 1L)
expect_equal(s[[1L]]$kind, "select")
expect_true(s[[1L]]$multiple)
expect_equal(s[[1L]]$choices, c("Species", "Petal.Width"))
