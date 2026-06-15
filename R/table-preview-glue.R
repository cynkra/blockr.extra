# blockr.extra-specific glue: opt-in S3 overrides that swap DT for the
# shared HTML table preview (canonical implementation in blockr.ui).
#
# The preview itself (build_html_table / table_page / html_table_result)
# lives in blockr.ui; this file only keeps the
# `options(blockr.html_table_preview = TRUE)` switch wired into
# blockr.core's data_block / transform_block render methods (captured and
# overridden in zzz.R).

#' Render block output with optional HTML table preview
#' @noRd
render_block_output_with_option <- function(x, result, session, original_method) {
  if (getOption("blockr.html_table_preview", FALSE)) {
    blockr.ui::html_table_result(result, x, session)
  } else if (!is.null(original_method)) {
    original_method(x, result, session)
  }
}

#' Render block UI with optional HTML table preview
#' @noRd
render_block_ui_with_option <- function(id, x, original_method, ...) {
  if (getOption("blockr.html_table_preview", FALSE)) {
    shiny::tagList(shiny::uiOutput(shiny::NS(id, "result")))
  } else if (!is.null(original_method)) {
    original_method(id, x, ...)
  }
}

#' @export
block_output.data_block <- function(x, result, session) {
  render_block_output_with_option(x, result, session, get_original_block_output_data_block())
}

#' @export
block_ui.data_block <- function(id, x, ...) {
  render_block_ui_with_option(id, x, get_original_block_ui_data_block(), ...)
}

#' @export
block_output.transform_block <- function(x, result, session) {
  render_block_output_with_option(x, result, session, get_original_block_output_transform_block())
}

#' @export
block_ui.transform_block <- function(id, x, ...) {
  render_block_ui_with_option(id, x, get_original_block_ui_transform_block(), ...)
}
