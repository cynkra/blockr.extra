#' Register Extra Blocks
#'
#' Registers the experimental function blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks new_block_args new_block_arg
#'   arg_string
register_extra_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_function_block",
      "new_function_var_block",
      "new_async_function_block",
      "new_broom_summary_block",
      "new_compare_block",
      "new_search_block",
      "new_latest_block"
    ),
    name = c(
      "Function block",
      "Function Var block",
      "Async Function block",
      "Broom Summary",
      "Compare",
      "Search",
      "Latest"
    ),
    description = c(
      "Transform data with a custom R function in a CodeMirror editor (syntax highlighting, autocomplete, inline AI diff). UI auto-generated from function arguments.",
      "Transform multiple data frames (...) with a custom R function. UI auto-generated from function arguments.",
      "Transform data with a custom R function asynchronously. Requires mirai daemons. Click Run to execute.",
      "Model summary using broom (tidy/glance/augment). Works with any broom-compatible model.",
      "Compare two data frames on key columns and compute diff metrics on measurement columns.",
      "Filter rows by case-insensitive substring match across all columns.",
      "Forward the value of whichever variadic input most recently changed (latest-wins merge / switch). Bridges multiple drill-down charts into one downstream block."
    ),
    category = c(
      "transform",
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
      "search",
      "shuffle"
    ),
    guidance = c(
      # new_function_block:
      # Authored once in inst/prompts/function-block.md; see function_block_prompt().
      tryCatch(
        function_block_prompt(),
        error = function(e) paste(
          "Write `fn` as `function(data, ...)`; every extra argument needs a",
          "default whose type picks the UI control (list() -> multi-select,",
          "c() -> single-select). Use base pipe |> and namespace-prefix calls."
        )
      ),
      # new_function_var_block:
      paste(
        "Write a complete R function as a string. The function receives '...' (any number of data frames) as its first argument.",
        "\n\nR coding rules: always use the base pipe |> (never %>%).",
        "Namespace-prefix all functions except base and stats (e.g. dplyr::bind_rows(), stringr::str_detect())."
      ),
      # new_async_function_block:
      "",
      # new_broom_summary_block:
      "",
      # new_compare_block:
      "",
      # new_search_block:
      "",
      # new_latest_block:
      ""
    ),
    arguments = list(
      # new_function_block:
      new_block_args(
        fn = new_block_arg(
          "A string of R code that evaluates to a function. The function must have 'data' as its first argument (the input data frame). Additional arguments with defaults become UI widgets.",
          # MULTI-LINE and indented (anchors readable output, not one-liners) and
          # demonstrates BOTH a c() single-select (sort_by) AND a list()
          # multi-select (keep) so the model has the multi-select pattern to copy.
          example = paste(
            "function(data,",
            "         sort_by = c('Sepal.Length' = 'Sepal.Length', 'Sepal.Width' = 'Sepal.Width'),",
            "         keep = list('Sepal.Length' = 'Sepal.Length', 'Species' = 'Species'),",
            "         n = 6L) {",
            "  data <- data[order(data[[sort_by]]), unlist(keep), drop = FALSE]",
            "  utils::head(data, n)",
            "}",
            sep = "\n"
          ),
          type = arg_string()
        )
      ),
      # new_function_var_block:
      new_block_args(
        fn = new_block_arg(
          "A string of R code that evaluates to a function. The function must have '...' as its first argument (variadic data frame inputs). Additional arguments with defaults become UI widgets.",
          example = "function(..., .id = NULL) { dplyr::bind_rows(..., .id = .id) }",
          type = arg_string()
        )
      ),
      # new_async_function_block:
      NULL,
      # new_broom_summary_block:
      NULL,
      # new_compare_block:
      NULL,
      # new_search_block:
      NULL,
      # new_latest_block:
      NULL
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
