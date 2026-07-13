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
      "new_latest_block",
      "new_labeler_block",
      "new_ctrl_filter_block"
    ),
    name = c(
      "Function block",
      "Function Var block",
      "Async Function block",
      "Broom Summary",
      "Compare",
      "Search",
      "Latest",
      "Labeler",
      "Cohort filter sender"
    ),
    description = c(
      "Transform data with a custom R function in a CodeMirror editor (syntax highlighting, autocomplete, inline AI diff). UI auto-generated from function arguments.",
      "Transform multiple data frames (...) with a custom R function. UI auto-generated from function arguments.",
      "Transform data with a custom R function asynchronously. Requires mirai daemons. Click Run to execute.",
      "Model summary using broom (tidy/glance/augment). Works with any broom-compatible model.",
      "Compare two data frames on key columns and compute diff metrics on measurement columns.",
      "Filter rows by case-insensitive substring match across all columns.",
      "Forward the value of whichever variadic input most recently changed (latest-wins merge / switch). Bridges multiple drill-down charts into one downstream block.",
      "Add or edit column labels (the `attr(col, \"label\")` attribute shown in column pickers and table headers). Empty label removes it.",
      "Push the drilled-to condition of an upstream table, chart or tile into a value filter block elsewhere on the board (no data link). Click a level to narrow the cohort, re-click to clear. Passes its input through unchanged."
    ),
    category = c(
      "transform",
      "transform",
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
      "shuffle",
      "tag",
      "funnel"
    ),
    guidance = c(
      # new_function_block:
      # Authored once in inst/prompts/function-block.md; see function_block_prompt().
      tryCatch(
        function_block_prompt(),
        error = function(e) paste(
          "Write `fn` as `function(data, ...)`; every extra argument needs a",
          "default whose type picks the UI control (list() -> multi-select,",
          "c() -> single-select). Prefer dplyr verbs chained with the base",
          "pipe |>; namespace-prefix calls (dplyr::filter()) and use",
          ".data[[col]] for string-valued column parameters."
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
      "",
      # new_labeler_block:
      paste(
        "Set `labels` to a named list mapping existing column names to",
        "human-readable label strings. Use an empty string to remove a",
        "column's label. Columns not present in the data are ignored."
      ),
      # new_ctrl_filter_block:
      paste(
        "Set `target` to the block id of the value filter block to control,",
        "and `table` to the table in its `dm` the conditions apply to.",
        "Leave `columns` empty when the upstream is a table or summary block",
        "(its drilled output carries the ARD identity the claim is read off).",
        "Set `columns` to the chart's drill column(s) when the upstream is a",
        "chart or tile, whose drilled output is a plain row subset."
      )
    ),
    arguments = list(
      # new_function_block:
      new_block_args(
        fn = new_block_arg(
          "A string of R code that evaluates to a function. The function must have 'data' as its first argument (the input data frame). Additional arguments with defaults become UI widgets.",
          # MULTI-LINE and indented (anchors readable output, not one-liners),
          # demonstrates BOTH a c() single-select (sort_by) AND a list()
          # multi-select (keep) so the model has the multi-select pattern to
          # copy, and is written in piped dplyr style (the preferred style; see
          # inst/prompts/function-block.md) incl. the .data[[col]]/all_of()
          # patterns for string-valued parameters. Keep in sync with the worked
          # example at the end of that prompt file.
          example = paste(
            "function(data,",
            "         sort_by = c('Sepal length (cm)' = 'Sepal.Length', 'Sepal width (cm)' = 'Sepal.Width'),",
            "         keep = list('Sepal length (cm)' = 'Sepal.Length', 'Flower species' = 'Species'),",
            "         n = 6L) {",
            "  data |>",
            "    dplyr::arrange(.data[[sort_by]]) |>",
            "    dplyr::select(dplyr::all_of(unname(unlist(keep)))) |>",
            "    dplyr::slice_head(n = n)",
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
      NULL,
      # new_labeler_block:
      # `labels` is an arbitrary-key map (column name -> label), which has
      # no JSON-Schema subset — left untyped like blockr.dplyr's `renames`.
      new_block_args(
        labels = new_block_arg(
          "Named list mapping column names to label strings. An empty string removes the column's label.",
          example = 'list(mpg = "Miles per gallon", cyl = "Number of cylinders")'
        )
      ),
      # new_ctrl_filter_block:
      new_block_args(
        target = new_block_arg(
          "Block id of the value filter block to send the drilled cohort to. Empty means not configured: the block passes data through and sends nothing.",
          example = "cohort_filter",
          type = arg_string()
        ),
        table = new_block_arg(
          "Name of the table in the target filter's `dm` that the conditions apply to.",
          example = "adsl",
          type = arg_string()
        ),
        columns = new_block_arg(
          "Source columns a click may claim. Leave empty when the upstream is a table or summary block; set it to the chart's drill column(s) when the upstream is a chart or tile.",
          example = 'c("SEX")'
        ),
        label = new_block_arg(
          "Name of the target as shown on the receipt. Defaults to the target block id.",
          example = "Cohort",
          type = arg_string()
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
