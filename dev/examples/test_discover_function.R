# Test discover_block_args() for function blocks
#
# Standalone script to test AI discovery of function block parameters.
# Run from the blockr.extra directory.

pkgload::load_all("../blockr.core")
pkgload::load_all("../blockr.ai")
pkgload::load_all("../blockr.extra")

run_test <- function(test_num, prompt, data, data_name = deparse(substitute(data))) {
  cat(sprintf("\n\n=== Test %d: %s ===\n", test_num, prompt))
  cat(sprintf("Data: %s (%d rows x %d cols)\n", data_name, nrow(data), ncol(data)))

  result <- discover_block_args(
    prompt = prompt,
    block = new_function_block(),
    data = data,
    verbose = TRUE
  )

  cat("\n--- Result ---\n")
  cat("Success:", result$success, "\n")

  if (result$success) {
    cat("fn:\n")
    cat(result$args$fn, "\n")

    # Verify the function parses and has defaults
    fn <- eval(parse(text = result$args$fn))
    fmls <- formals(fn)
    cat("Formals:", paste(names(fmls), collapse = ", "), "\n")

    missing_defaults <- vapply(fmls[-1], function(x) identical(x, quote(expr = )), logical(1))
    if (any(missing_defaults)) {
      cat("WARNING: Parameters without defaults:", paste(names(fmls[-1])[missing_defaults], collapse = ", "), "\n")
    } else {
      cat("All parameters have defaults: OK\n")
    }

    # Try running the function
    tryCatch({
      out <- fn(data)
      cat("Output class:", paste(class(out), collapse = ", "), "\n")
      if (is.data.frame(out)) {
        cat("Output dim:", nrow(out), "x", ncol(out), "\n")
        print(utils::head(out, 3))
      } else {
        cat(utils::capture.output(print(utils::head(out))), sep = "\n")
      }
    }, error = function(e) {
      cat("Execution error:", conditionMessage(e), "\n")
    })
  } else {
    cat("Error:", result$error, "\n")
  }

  invisible(result)
}

# Test 1: Simple numeric default
r1 <- run_test(1, "show the first 3 rows", iris)

# Test 2: Data manipulation
r2 <- run_test(2, "filter to setosa and compute mean sepal length", iris)

# Test 3: Plot output
r3 <- run_test(3, "scatter plot of mpg vs hp", mtcars)

# Test 4: Model output
r4 <- run_test(4, "fit lm of mpg ~ wt + hp", mtcars)

# Test 5: Dropdown params via c()
r5 <- run_test(5, "regression block with dropdowns for response and predictors", iris)

cat("\n\n=== Summary ===\n")
results <- list(r1, r2, r3, r4, r5)
for (i in seq_along(results)) {
  cat(sprintf("Test %d: %s\n", i, if (results[[i]]$success) "PASS" else "FAIL"))
}
