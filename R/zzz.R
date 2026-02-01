# Store references to original blockr.core methods
.blockr_extra_env <- new.env(parent = emptyenv())

#' Override blockr.core methods with blockr.extra implementations
#'
#' Injects blockr.extra's block_output and block_ui methods for data_block
#' and transform_block into the blockr.core namespace. The overrides check
#' an option and fall back to the original methods when disabled.
#'
#' @noRd
override_blockr_methods <- function() {

  ns <- asNamespace("blockr.core")

  # Helper to store and override a method
  store_and_override <- function(method_name, new_method) {
    store_key <- paste0("original_", method_name)

    # Store original (only once)
    if (is.null(.blockr_extra_env[[store_key]])) {
      if (exists(method_name, envir = ns)) {
        .blockr_extra_env[[store_key]] <- get(method_name, envir = ns)
      }
    }

    # Override
    if (exists(method_name, envir = ns)) {
      unlockBinding(method_name, ns)
      assign(method_name, new_method, envir = ns)
      lockBinding(method_name, ns)
    }
  }

  # Override data_block methods

  store_and_override("block_output.data_block", block_output.data_block)
  store_and_override("block_ui.data_block", block_ui.data_block)

  # Override transform_block methods
  store_and_override("block_output.transform_block", block_output.transform_block)
  store_and_override("block_ui.transform_block", block_ui.transform_block)
}

#' Get original blockr.core method
#' @noRd
get_original_method <- function(method_name) {
  .blockr_extra_env[[paste0("original_", method_name)]]
}

#' @noRd
get_original_block_output_data_block <- function() {
  get_original_method("block_output.data_block")
}

#' @noRd
get_original_block_ui_data_block <- function() {
  get_original_method("block_ui.data_block")
}

#' @noRd
get_original_block_output_transform_block <- function() {
  get_original_method("block_output.transform_block")
}

#' @noRd
get_original_block_ui_transform_block <- function() {
  get_original_method("block_ui.transform_block")
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
