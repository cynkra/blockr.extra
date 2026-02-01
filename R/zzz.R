# Store references to original blockr.core methods
.blockr_extra_env <- new.env(parent = emptyenv())

#' Override blockr.core methods with blockr.extra implementations
#'
#' Injects blockr.extra's block_output.data_block and block_ui.data_block
#' into the blockr.core namespace. The overrides check an option and
#' fall back to the original methods when disabled.
#'
#' @noRd
override_blockr_methods <- function() {

  ns <- asNamespace("blockr.core")

  # Store original methods (only once)
  if (is.null(.blockr_extra_env$original_block_output_data_block)) {
    if (exists("block_output.data_block", envir = ns)) {
      .blockr_extra_env$original_block_output_data_block <- get(
        "block_output.data_block",
        envir = ns
      )
    }
  }

  if (is.null(.blockr_extra_env$original_block_ui_data_block)) {
    if (exists("block_ui.data_block", envir = ns)) {
      .blockr_extra_env$original_block_ui_data_block <- get(
        "block_ui.data_block",
        envir = ns
      )
    }
  }

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

#' Get original blockr.core block_output.data_block method
#' @noRd
get_original_block_output_data_block <- function() {
  .blockr_extra_env$original_block_output_data_block
}

#' Get original blockr.core block_ui.data_block method
#' @noRd
get_original_block_ui_data_block <- function() {
  .blockr_extra_env$original_block_ui_data_block
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
