#' Register Extra Blocks
#'
#' Registers the experimental function blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks
register_extra_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_function_block",
      "new_function_xy_block",
      "new_function_var_block",
      "new_async_function_block",
      "new_broom_summary_block"
    ),
    name = c(
      "Function block",
      "Function XY block",
      "Function Var block",
      "Async Function block",
      "Broom Summary"
    ),
    description = c(
      "Transform data with a custom R function. UI auto-generated from function arguments.",
      "Transform two data frames (x, y) with a custom R function. UI auto-generated from function arguments.",
      "Transform multiple data frames (...) with a custom R function. UI auto-generated from function arguments.",
      "Transform data with a custom R function asynchronously. Requires mirai daemons. Click Run to execute.",
      "Model summary using broom (tidy/glance/augment). Works with any broom-compatible model."
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "code-slash",
      "code-slash",
      "code-slash",
      "hourglass-split",
      "clipboard-data"
    ),
    arguments = list(
      # new_function_block:
      structure(
        c(
          fn = "A string of R code that evaluates to a function. The function must have 'data' as its first argument (the input data frame). Additional arguments with defaults become UI widgets."
        ),
        examples = list(
          fn = "function(data, n = 6L) { utils::head(data, n) }"
        ),
        prompt = "Write a complete R function as a string. The function receives 'data' (a data frame) as its first argument. Use default values for additional parameters to generate UI controls."
      ),
      # new_function_xy_block:
      structure(
        c(
          fn = "A string of R code that evaluates to a function. The function must have 'x' as first and 'y' as second argument (two input data frames). Additional arguments with defaults become UI widgets."
        ),
        examples = list(
          fn = "function(x, y) { dplyr::left_join(x, y, by = 'name') }"
        ),
        prompt = "Write a complete R function as a string. The function receives 'x' and 'y' (two data frames) as its first two arguments."
      ),
      # new_function_var_block:
      structure(
        c(
          fn = "A string of R code that evaluates to a function. The function must have '...' as its first argument (variadic data frame inputs). Additional arguments with defaults become UI widgets."
        ),
        examples = list(
          fn = "function(..., .id = NULL) { dplyr::bind_rows(..., .id = .id) }"
        ),
        prompt = "Write a complete R function as a string. The function receives '...' (any number of data frames) as its first argument."
      ),
      # new_async_function_block:
      NULL,
      # new_broom_summary_block:
      NULL
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
