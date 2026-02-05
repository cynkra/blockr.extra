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
    package = utils::packageName(),
    overwrite = TRUE
  )
}
