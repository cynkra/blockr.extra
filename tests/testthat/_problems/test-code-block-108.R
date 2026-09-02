# Extracted from test-code-block.R:108

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
    'lv <- unique(data$Species)',
    'boom <- stop("should never run")',
    'x <- factor("setosa", lv)',
    sep = "\n"
  )
s <- specs_for(script)
expect_length(s, 1L)
