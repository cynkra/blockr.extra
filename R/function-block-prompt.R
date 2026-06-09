#' Base authoring prompt for function blocks
#'
#' Returns the canonical guidance the assistant uses to write the `fn` of a
#' [new_function_block()]: the `function(data, ...)` contract and the rule that
#' formal-argument defaults become UI controls (`list()` -> multi-select,
#' `c()` -> single-select, single scalars -> numeric/checkbox/text).
#'
#' Authored once here and reused: the registry entry for `new_function_block`
#' uses it directly, and downstream packages that specialise the function block
#' (e.g. the composer function block in \pkg{blockr.sandbox}) prepend it to their
#' own block-specific guidance so the base contract is never duplicated.
#'
#' @return A single character string.
#' @export
function_block_prompt <- function() {
  f <- system.file("prompts", "function-block.md", package = "blockr.extra")
  if (nzchar(f) && file.exists(f)) {
    return(paste(readLines(f, warn = FALSE), collapse = "\n"))
  }
  # Fallback when the installed package predates the prompt file.
  paste(
    "Write `fn` as `function(data, ...)`. `data` is the input.",
    "Every extra argument needs a default; its type picks the UI control:",
    "list(...) -> multi-select (all selected); c(...) -> single-select (first);",
    "single numeric -> number input; logical -> checkbox; character -> text.",
    "Use the base pipe |> and namespace-prefix non-base/stats calls.",
    "Explore the data (names(data), sort(unique(data$col))) before referencing columns."
  )
}
