#' Override blockr.core methods with blockr.extra implementations
#'
#' Injects blockr.extra's block_output.data_block and block_ui.data_block
#' into the blockr.core namespace to override the default implementations.
#'
#' @noRd
override_blockr_methods <- function() {
  ns <- asNamespace("blockr.core")

  # Override block_output.data_block
  if (exists("block_output.data_block", envir = ns)) {
    unlockBinding("block_output.data_block", ns)
    assign("block_output.data_block", block_output.data_block, envir = ns)
    lockBinding("block_output.data_block", ns)
  }

  # Override block_ui.data_block
  if (exists("block_ui.data_block", envir = ns)) {
    unlockBinding("block_ui.data_block", ns)
    assign("block_ui.data_block", block_ui.data_block, envir = ns)
    lockBinding("block_ui.data_block", ns)
  }
}

.onLoad <- function(libname, pkgname) {
  register_extra_blocks()
  override_blockr_methods()

  # Also override when blockr.dock loads (in case it resets methods)
  setHook(
    packageEvent("blockr.dock", "onLoad"),
    function(...) override_blockr_methods()
  )

  invisible(NULL)
}
