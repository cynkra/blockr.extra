# blockr.extra-specific glue that wires the shared table preview
# (table-preview.R) into blockr.core's block system.

#' HTML Table Preview for Data Frames
#'
#' Replaces DT output with a lightweight HTML table for data blocks,
#' transform blocks, and parser blocks.
#'
#' @param result The data frame result to display
#' @param block The block object
#' @param session Shiny session object
#'
#' @return A shiny renderUI object containing the HTML table
#'
#' @keywords internal
html_table_result <- function(result, block, session) {

  page_size <- tryCatch(
    blockr.core::get_board_option_or_default(
      "page_size",
      blockr.core::board_options(block),
      session
    ),
    error = function(e) 5L
  )

  html_table_render(result, session, page_size)
}

#' Render an Interactive HTML Table
#'
#' Stateless table renderer with server-side sorting and pagination.
#' Sort and page state are managed in the browser via Shiny inputs ---
#' no session$userData or observers required.
#'
#' @param result The data frame result to display
#' @param session Shiny session object
#' @param page_size Number of rows per page (default 5)
#'
#' @return A shiny renderUI object containing the HTML table
#'
#' @keywords internal
html_table_render <- function(result, session, page_size = 5L) {
  ns <- session$ns

  shiny::renderUI({
    tryCatch({
      sort_input <- session$input$blockr_table_sort
      current_sort <- if (!is.null(sort_input)) {
        list(col = sort_input$col, dir = sort_input$dir)
      } else {
        list(col = NULL, dir = "none")
      }

      page <- session$input$blockr_table_page
      page <- if (is.null(page)) 1L else as.integer(page)

      total_rows <- if (is.null(result)) 0L else nrow(result)
      max_page <- max(1L, ceiling(total_rows / page_size))
      page <- min(max(1L, page), max_page)

      sorted_result <- apply_table_sort(
        result,
        current_sort$col,
        current_sort$dir
      )

      start_row <- (page - 1L) * page_size + 1L
      end_row <- min(page * page_size, total_rows)
      dat <- if (total_rows > 0 && end_row >= start_row) {
        as.data.frame(dplyr::slice(sorted_result, start_row:end_row))
      } else {
        as.data.frame(sorted_result)
      }

      build_html_table(
        dat,
        total_rows,
        sort_state = current_sort,
        ns = ns,
        page = page,
        page_size = page_size
      )
    }, error = function(e) {
      shiny::tags$div(
        class = "blockr-table-error",
        style = "color: red; padding: 12px;",
        paste("Error rendering table:", conditionMessage(e))
      )
    })
  })
}


# --- S3 method overrides (opt-in via option) ---

#' Render block output with optional HTML table preview
#' @noRd
render_block_output_with_option <- function(x, result, session, original_method) {
  if (getOption("blockr.html_table_preview", FALSE)) {
    html_table_result(result, x, session)
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
