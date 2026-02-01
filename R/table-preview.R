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
    error = function(e) 10L  # fallback default
  )

  shiny::renderUI({
    tryCatch({
      dat <- as.data.frame(utils::head(result, page_size))
      total_rows <- if (is.null(result)) 0L else nrow(result)
      build_html_table(dat, total_rows)
    }, error = function(e) {
      shiny::tags$div(
        class = "blockr-table-error",
        style = "color: red; padding: 12px;",
        paste("Error rendering table:", conditionMessage(e))
      )
    })
  })
}

#' Build HTML Table
#'
#' Constructs the HTML table structure with type indicators and row numbers.
#'
#' @param dat Data frame to display (already subset to n_rows)
#' @param total_rows Total number of rows in original data
#'
#' @return A shiny tagList containing the table HTML
#'
#' @keywords internal
build_html_table <- function(dat, total_rows) {
  n_showing <- nrow(dat)
  n_cols <- ncol(dat)

  # Handle empty data frame
  if (n_cols == 0) {
    return(
      shiny::tagList(
        shiny::tags$div(
          class = "blockr-table-container",
          shiny::tags$div(
            class = "blockr-table-info",
            "Empty data frame (0 columns)"
          )
        ),
        table_preview_css()
      )
    )
  }

  # Build header row
  header_cells <- list(
    shiny::tags$th(class = "blockr-row-number", "")
  )

  for (col_name in names(dat)) {
    type_label <- col_type_label(dat[[col_name]])
    header_cells <- c(header_cells, list(
      shiny::tags$th(
        col_name,
        shiny::tags$span(class = "blockr-type-label", type_label)
      )
    ))
  }

  # Build body rows
  body_rows <- list()

  if (n_showing > 0) {
    for (i in seq_len(n_showing)) {
      row_cells <- list(
        shiny::tags$td(class = "blockr-row-number", i)
      )

      for (col_name in names(dat)) {
        val <- dat[[col_name]][i]
        is_numeric <- is.numeric(dat[[col_name]])
        cell_class <- if (is_numeric) "blockr-td-numeric" else NULL
        formatted_val <- format_cell_value(val)

        row_cells <- c(row_cells, list(
          shiny::tags$td(class = cell_class, formatted_val)
        ))
      }

      body_rows <- c(body_rows, list(
        do.call(shiny::tags$tr, row_cells)
      ))
    }
  }

  # Build info text
  info_text <- if (total_rows == 0) {
    "No rows"
  } else if (n_showing >= total_rows) {
    sprintf("Showing all %d rows", total_rows)
  } else {
    sprintf("Showing %d of %d rows", n_showing, total_rows)
  }

  # Assemble table
  shiny::tagList(
    shiny::tags$div(
      class = "blockr-table-container",
      shiny::tags$table(
        class = "blockr-table",
        shiny::tags$thead(
          do.call(shiny::tags$tr, header_cells)
        ),
        do.call(shiny::tags$tbody, body_rows)
      ),
      shiny::tags$div(class = "blockr-table-info", info_text)
    ),
    table_preview_css()
  )
}

#' Column Type Label
#'
#' Returns tibble-style type labels for column types.
#'
#' @param x A vector
#'
#' @return A character string like "<chr>", "<int>", "<dbl>", etc.
#'
#' @keywords internal
col_type_label <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    "<dttm>"
  } else if (inherits(x, "Date")) {
    "<date>"
  } else if (is.factor(x)) {
    "<fct>"
  } else if (is.logical(x)) {
    "<lgl>"
  } else if (is.integer(x)) {
    "<int>"
  } else if (is.numeric(x)) {
    "<dbl>"
  } else if (is.character(x)) {
    "<chr>"
  } else if (is.list(x)) {
    "<list>"
  } else {
    paste0("<", class(x)[1], ">")
  }
}

#' Format Cell Value
#'
#' Formats a single cell value for display.
#'
#' @param x A single value
#' @param max_chars Maximum characters before truncation (default 50)
#'
#' @return A formatted character string
#'
#' @keywords internal
format_cell_value <- function(x, max_chars = 50) {
  if (is.na(x)) {
    return("NA")
  }

  if (is.list(x)) {
    return("<list>")
  }

  val <- as.character(x)

  if (nchar(val) > max_chars) {
    val <- paste0(substr(val, 1, max_chars - 3), "...")
  }

  val
}

#' Table Preview CSS
#'
#' Returns inline CSS for table styling. Uses CSS variables from blockr.dock
#' when available, with fallback values for standalone use.
#'
#' @return A shiny tags$style element
#'
#' @keywords internal
table_preview_css <- function() {
  shiny::tags$style(shiny::HTML("
    .blockr-table-container {
      background: white;
      width: 100%;
    }

    .blockr-table {
      border-collapse: collapse;
      width: 100%;
      font-size: var(--blockr-font-size-base, 0.875rem);
    }

    .blockr-table thead tr {
      border-bottom: 1px solid var(--blockr-color-border, #e5e7eb);
    }

    .blockr-table th {
      text-align: left;
      padding: 12px 16px;
      font-weight: var(--blockr-font-weight-medium, 500);
      color: var(--blockr-color-text-primary, #111827);
      vertical-align: bottom;
    }

    .blockr-table th.blockr-row-number {
      width: 64px;
      text-align: center;
    }

    .blockr-type-label {
      display: block;
      font-size: 11px;
      font-weight: var(--blockr-font-weight-normal, 400);
      color: var(--blockr-color-text-subtle, #9ca3af);
      margin-top: 2px;
    }

    .blockr-table tbody tr {
      border-bottom: 1px solid var(--blockr-grey-100, #f3f4f6);
      transition: background-color 0.15s ease;
    }

    .blockr-table tbody tr:hover {
      background-color: var(--blockr-color-bg-subtle, #f9fafb);
    }

    .blockr-table td {
      padding: 12px 16px;
      font-size: var(--blockr-font-size-base, 0.875rem);
      color: var(--blockr-color-text-primary, #111827);
      max-width: 200px;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }

    .blockr-table .blockr-row-number {
      text-align: center;
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-color-text-subtle, #9ca3af);
    }

    .blockr-table .blockr-td-numeric {
      text-align: right;
      font-variant-numeric: tabular-nums;
    }

    .blockr-table-info {
      padding: 12px 16px;
      font-size: var(--blockr-font-size-xs, 0.75rem);
      color: var(--blockr-color-text-muted, #6b7280);
    }
  "))
}


#' @export
block_output.data_block <- function(x, result, session) {
  html_table_result(result, x, session)
}

#' @export
block_ui.data_block <- function(id, x, ...) {
  shiny::tagList(shiny::uiOutput(shiny::NS(id, "result")))
}
