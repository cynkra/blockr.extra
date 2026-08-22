#' Create a DataTable widget with board options
#'
#' Creates a DT::datatable with row limiting based on board options.
#' This mirrors the behavior of blockr.core's dt_result but works
#' within a renderUI context for dynamic output switching.
#'
#' @param result Data frame to display
#' @param block The block object (for getting default board options)
#' @param session Shiny session
#' @return A DT::datatable widget
#' @noRd
dt_datatable <- function(result, block, session) {
  info_string <- function(total) {
    paste0(
      "\"Showing \" + start + \" to \" + end + \" of ",
      if (is.null(total)) "0" else if (is.na(total)) "??" else total,
      " entries\""
    )
  }

  need_pagination <- function(dat_row, show_row, page) {
    if (is.na(dat_row)) show_row > page else min(dat_row, show_row) > page
  }

  # DT's three options are no longer guaranteed to be anywhere. Since
  # blockr.core #292 a data or transform block contributes exactly the board
  # options the active `tabular_display` declares, so `n_rows`, `page_size`
  # and `filter_rows` are all absent unless that display is `dt_display` --
  # and a function block never carried them at all, being built on new_block()
  # (its board_options() are just the four defaults). Without a fallback,
  # get_board_option_or_default() reaches board_option_value(NULL) and errors,
  # taking the whole preview with it. Append the ctor defaults so there is
  # always something to land on; combine_board_options() keeps the first of a
  # duplicated id, so a board that does carry them still wins.
  default_opts <- blockr.core::combine_board_options(
    blockr.core::board_options(block),
    blockr.core::new_n_rows_option(),
    blockr.core::new_page_size_option(),
    blockr.core::new_filter_rows_option()
  )

  rows <- blockr.core::get_board_option_or_default("n_rows", default_opts, session)
  page <- blockr.core::get_board_option_or_default("page_size", default_opts, session)

  dom <- "rti"

  if (!is.null(result) && need_pagination(nrow(result), rows, page)) {
    dom <- paste0(dom, "p")
  }

  if (blockr.core::get_board_option_or_default("filter_rows", default_opts, session)) {
    dom <- paste0("f", dom)
  }

  opts <- list(
    processing = FALSE,
    infoCallback = DT::JS(
      "function(settings, start, end, max, total, pre) {",
      "let res = ", info_string(nrow(result)), ";",
      "return res;",
      "}"
    ),
    dom = dom,
    pageLength = page,
    ordering = FALSE
  )

  dat <- as.data.frame(utils::head(result, rows))
  dt <- DT::datatable(
    dat,
    selection = "none",
    options = opts
  )

  # Skip formatStyle for empty dataframes (0 columns)
  if (ncol(dat) > 0) {
    DT::formatStyle(
      dt,
      columns = names(dat),
      whiteSpace = "pre-wrap"
    )
  } else {
    dt
  }
}
