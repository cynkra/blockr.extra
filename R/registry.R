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
      "new_broom_summary_block",
      "new_compare_block",
      "new_search_block"
    ),
    name = c(
      "Function block",
      "Function XY block",
      "Function Var block",
      "Async Function block",
      "Broom Summary",
      "Compare",
      "Search"
    ),
    description = c(
      "Transform data with a custom R function. UI auto-generated from function arguments.",
      "Transform two data frames (x, y) with a custom R function. UI auto-generated from function arguments.",
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
      "transform",
      "transform"
    ),
    icon = c(
      "code-slash",
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
        prompt = paste(
          "Write the value of fn as a SINGLE-LINE R function string (no newlines inside the string — this is critical because the value is embedded in JSON).",
          "The function MUST have 'data' as its first argument.",
          "ALL additional parameters MUST have default values — a parameter without a default will crash the app.",
          "Default value types map to UI widgets: character vector with multiple elements c('A' = 'a', 'B' = 'b') -> dropdown; single numeric -> number input; single logical -> checkbox; single character string -> text input.",
          "For dropdown parameters, ALWAYS use a named c() vector where names are display labels and values are the actual values, e.g. column = c('Sepal.Length' = 'Sepal.Length', 'Petal.Width' = 'Petal.Width'). An unnamed c() vector will NOT create a dropdown — it will break the function.",
          "Use column names from the actual data provided for any column-selection parameters.",
          "Wrap the entire function body in curly braces on one line, separating statements with semicolons.",
          "\n\nR coding rules: always use the base pipe |> (never %>%).",
          "Namespace-prefix all functions except base and stats (e.g. dplyr::filter(), stringr::str_detect()).",
          "\n\nData exploration: explore the data structure (e.g. str(data), names(data)) to write a function",
          "that correctly references available columns and handles their data types.",
          "When creating dropdown parameters, explore unique values (e.g. unique(data$col) or sort(unique(data$col)))",
          "so you can populate the c() vector with ALL actual values from the data, not just the ones visible in the preview."
        )
      ),
      # new_function_xy_block:
      structure(
        c(
          fn = "A string of R code that evaluates to a function. The function must have 'x' as first and 'y' as second argument (two input data frames). Additional arguments with defaults become UI widgets."
        ),
        examples = list(
          fn = "function(x, y) { dplyr::left_join(x, y, by = 'name') }"
        ),
        prompt = paste(
          "Write a complete R function as a string. The function receives 'x' and 'y' (two data frames) as its first two arguments.",
          "\n\nR coding rules: always use the base pipe |> (never %>%).",
          "Namespace-prefix all functions except base and stats (e.g. dplyr::left_join(), stringr::str_detect())."
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
