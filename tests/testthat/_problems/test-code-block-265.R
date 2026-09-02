# Extracted from test-code-block.R:265

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
    'keep <- c("Sepal.Length", "Petal.Length", "Missing")',
    'keep <- intersect(keep, names(data))',
    '',
    'col <- factor(keep, levels = keep)',
    '',
    'data[, as.character(col), drop = FALSE]',
    sep = "\n"
  )
p <- cb_parse(script)
expect_equal(cb_shadowed(p), "keep")
specs <- cb_specs(p, iris)
expect_equal(vapply(specs, `[[`, character(1L), "name"), "col")
expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 4)
