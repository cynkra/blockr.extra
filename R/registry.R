#' Register Extra Blocks
#'
#' Registers the experimental function blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string
register_extra_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_function_block",
      "new_code_block",
      "new_function_var_block",
      "new_async_function_block",
      "new_broom_summary_block",
      "new_compare_block",
      "new_search_block",
      "new_latest_block",
      "new_labeler_block"
    ),
    name = c(
      "Function block",
      "Code block",
      "Function Var block",
      "Async Function block",
      "Broom Summary",
      "Compare",
      "Search",
      "Latest",
      "Labeler"
    ),
    description = c(
      "Transform data with a custom R function in a CodeMirror editor (syntax highlighting, autocomplete, inline AI diff). UI auto-generated from function arguments.",
      "Transform data with a plain R script (no wrapper function). Assignments at the TOP of the script become controls: a factor renders a dropdown over its levels, intersect()/unique() and friends a multi-select over the columns or values the data has, a number a spin box, TRUE/FALSE a checkbox. A name starting with a dot stays private. Exports as idiomatic R with the current values written in.",
      "Transform multiple data frames (...) with a custom R function. UI auto-generated from function arguments.",
      "Transform data with a custom R function asynchronously. Requires mirai daemons. Click Run to execute.",
      "Model summary using broom (tidy/glance/augment). Works with any broom-compatible model.",
      "Compare two data frames on key columns and compute diff metrics on measurement columns.",
      "Filter rows by case-insensitive substring match across all columns.",
      "Forward the value of whichever variadic input most recently changed (latest-wins merge / switch). Bridges multiple drill-down charts into one downstream block.",
      "Add or edit column labels (the `attr(col, \"label\")` attribute shown in column pickers and table headers). Empty label removes it."
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
      "braces",
      "code-slash",
      "hourglass-split",
      "clipboard-data",
      "arrow-left-right",
      "search",
      "shuffle",
      "tag"
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
      # new_code_block:
      paste(
        "Write `script` as an ordinary R script that transforms `data` (the",
        "reserved name for the incoming data frame) and ends with the result.",
        "Do NOT wrap it in a function and do NOT call one.",
        "\n\nControls are declared in the HEADER: the run of assignments the",
        "script opens with, before the first line of real code.",
        "To offer the user a control, assign a plain value there:",
        "a factor renders a dropdown (its LEVELS are the choices, and a value of",
        "length > 1 makes it a multi-select), a bare number a spin box, a string",
        "a text box, TRUE/FALSE a checkbox, as.Date() a date picker. Draw the",
        "levels from the data where that is what you mean, e.g.",
        "`site <- factor(\"Basel\", unique(data$site))`. A literal is always the",
        "VALUE, never the choice list.",
        "\n\nWhere the choices are only known from the data, assign a pool:",
        "intersect(), setdiff(), union(), unique(), sort(), rev(), names(),",
        "colnames() and levels() render a multi-select over their result, e.g.",
        "`vars <- intersect(names(data), c(\"AGE\", \"SEX\"))`.",
        "\n\nEverything else is code: an assignment whose right-hand side is a",
        "pipe or any other call is a local variable, not a control. A",
        "declaration is also NOT a control when the script assigns the same name",
        "again (only the last assignment can be one), or when it is written",
        "below the header.",
        "\n\nA name starting with a DOT is never a control. Use that for a header",
        "line that works something out from the data, e.g.",
        "`.cands <- c(\"AGE\", \"SEX\")` above",
        "`vars <- intersect(.cands, names(data))`.",
        "\n\nR coding rules: prefer dplyr/tidyr chained with the base pipe |>",
        "(never %>%). Namespace-prefix every call except base and stats",
        "(dplyr::filter(), tidyr::pivot_longer())."
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
      )
    ),
    arguments = list(
      # new_function_block:
      new_arg_specs(
        fn = new_arg_spec(
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
      # new_code_block:
      new_arg_specs(
        script = new_arg_spec(
          "A string of R code transforming `data` into the result. Top-level assignments of plain values (literals, c(), factor(), as.Date()) become UI controls; every other statement is code.",
          # Anchors the two things models get wrong: no function wrapper, and a
          # factor (not a bare character vector) is how a dropdown is declared.
          example = paste(
            "species <- factor(\"setosa\", unique(data$Species))",
            "n <- 10",
            "",
            "data |>",
            "  dplyr::filter(Species == species) |>",
            "  dplyr::slice_head(n = n)",
            sep = "\n"
          ),
          type = arg_string()
        )
      ),
      # new_function_var_block:
      new_arg_specs(
        fn = new_arg_spec(
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
      new_arg_specs(
        labels = new_arg_spec(
          "Named list mapping column names to label strings. An empty string removes the column's label.",
          example = 'list(mpg = "Miles per gallon", cyl = "Number of cylinders")'
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
