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
      "new_function_var_block"
    ),
    name = c(
      "Function block",
      "Function XY block",
      "Function Var block"
    ),
    description = c(
      "Transform data with a custom R function. UI auto-generated from function arguments.",
      "Transform two data frames (x, y) with a custom R function. UI auto-generated from function arguments.",
      "Transform multiple data frames (...) with a custom R function. UI auto-generated from function arguments."
    ),
    category = c(
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "code-slash",
      "code-slash",
      "code-slash"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
