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
      "new_function_var_block",
      "new_async_function_block",
      "new_broom_summary_block",
      "new_compare_block",
      "new_search_block"
    ),
    name = c(
      "Function block",
      "Function Var block",
      "Async Function block",
      "Broom Summary",
      "Compare",
      "Search"
    ),
    description = c(
      "Transform data with a custom R function in a CodeMirror editor (syntax highlighting, autocomplete, inline AI diff). UI auto-generated from function arguments.",
      "Transform multiple data frames (...) with a custom R function. UI auto-generated from function arguments.",
      "Transform data with a custom R function asynchronously. Requires mirai daemons. Click Run to execute.",
      "Model summary using broom (tidy/glance/augment). Works with any broom-compatible model.",
      "Compare two data frames on key columns and compute diff metrics on measurement columns.",
      "Filter rows by case-insensitive substring match across all columns."
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "code-slash",
      "code-slash",
      "hourglass-split",
      "clipboard-data",
      "arrow-left-right",
      "search"
    ),
    arguments = list(
      # new_function_block:
      structure(
        c(
          fn = "A string of R code that evaluates to a function. The function must have 'data' as its first argument (the input data frame). Additional arguments with defaults become UI widgets."
        ),
        examples = list(
          fn = "function(data, column = c('Sepal.Length' = 'Sepal.Length', 'Sepal.Width' = 'Sepal.Width'), n = 6L, descending = FALSE) { data <- data[order(data[[column]], decreasing = descending), ]; utils::head(data, n) }"
        ),
        # Authored once in inst/prompts/function-block.md; see function_block_prompt().
        prompt = tryCatch(
          function_block_prompt(),
          error = function(e) paste(
            "Write `fn` as `function(data, ...)`; every extra argument needs a",
            "default whose type picks the UI control (list() -> multi-select,",
            "c() -> single-select). Use base pipe |> and namespace-prefix calls."
          )
        )
      ),
      # new_function_var_block:
      structure(
        c(
          fn = "A string of R code that evaluates to a function. The function must have '...' as its first argument (variadic data frame inputs). Additional arguments with defaults become UI widgets."
        ),
        examples = list(
          fn = "function(..., .id = NULL) { dplyr::bind_rows(..., .id = .id) }"
        ),
        prompt = paste(
          "Write a complete R function as a string. The function receives '...' (any number of data frames) as its first argument.",
          "\n\nR coding rules: always use the base pipe |> (never %>%).",
          "Namespace-prefix all functions except base and stats (e.g. dplyr::bind_rows(), stringr::str_detect())."
        )
      ),
      # new_async_function_block:
      NULL,
      # new_broom_summary_block:
      NULL,
      # new_compare_block:
      NULL,
      # new_search_block:
      NULL
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
