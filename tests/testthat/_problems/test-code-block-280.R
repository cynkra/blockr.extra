# Extracted from test-code-block.R:280

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
specs <- specs_for(paste(
    'keep <- c("Sepal.Length", "Nope")',
    'keep <- intersect(keep, names(data))',
    'col <- factor(keep, levels = keep)',
    'data[, as.character(col), drop = FALSE]',
    sep = "\n"
  ))
expect_length(specs, 1L)
expect_null(specs[[1L]]$error)
expect_equal(specs[[1L]]$choices, "Sepal.Length")
