#' Labeler block
#'
#' Edits column labels — the `attr(col, "label")` attribute that blockr
#' surfaces throughout the ecosystem: as secondary text in column pickers
#' (blockr.dplyr), as table headers (blockr.viz), and in the table preview
#' (blockr.ui). Each row pairs a column picker with a text input holding the
#' column's label; columns without a label can be given one, existing labels
#' can be edited, and clearing the text removes the label.
#'
#' The UI follows the blockr.dplyr design system (JS-driven rows with the
#' shared `Blockr.Select` column dropdown), reusing the exported blockr.dplyr
#' dependencies and column-metadata protocol.
#'
#' @param labels Named list (or named character vector) mapping column names
#'   to label strings. An empty string removes the column's label.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_labeler_block(labels = list(mpg = "Miles per gallon")),
#'     data = list(data = mtcars)
#'   )
#' }
#'
#' @export
new_labeler_block <- function(labels = list(), ...) {
  labels <- as_label_list(labels)

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_labels <- shiny::reactiveVal(labels)

          # Push column metadata (names + existing labels) to JS on every
          # data change — same protocol as the blockr.dplyr blocks.
          shiny::observeEvent(data(), {
            session$sendCustomMessage(
              "labeler-columns",
              list(
                id = session$ns("labeler_input"),
                columns = blockr.dplyr::build_column_picker_meta(data())
              )
            )
          })

          self_write <- new.env(parent = emptyenv())
          self_write$active <- FALSE

          # JS -> R
          shiny::observeEvent(input$labeler_input, {
            self_write$active <- TRUE
            r_labels(as_label_list(input$labeler_input$labels))
          })

          # R -> JS: restore / external control. Fires on init (no
          # ignoreInit) so the constructor state reaches the JS class;
          # blockr-core.js queues it until the element binds.
          shiny::observeEvent(r_labels(), {
            if (self_write$active) {
              self_write$active <- FALSE
            } else {
              session$sendCustomMessage(
                "labeler-block-update",
                list(
                  id = session$ns("labeler_input"),
                  state = list(labels = r_labels())
                )
              )
            }
          })

          list(
            expr = shiny::reactive(make_labeler_expr(r_labels())),
            state = list(labels = r_labels)
          )
        }
      )
    },
    function(id) {
      shiny::tagList(
        blockr.dplyr::blockr_core_js_dep(),
        blockr.dplyr::blockr_blocks_css_dep(),
        blockr.dplyr::blockr_select_dep(),
        labeler_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(
            id = shiny::NS(id, "labeler_input"),
            class = "labeler-block-container"
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Labeler block requires a data frame as input.")
      }
    },
    expr_type = "bquoted",
    allow_empty_state = TRUE,
    external_ctrl = TRUE,
    class = "labeler_block",
    ...
  )
}

#' Set column labels on a data frame
#'
#' Sets `attr(col, "label")` for each entry of `labels` whose name is a
#' column of `data`. Entries for absent columns are skipped silently —
#' labels are cosmetic and the upstream data shape may change without the
#' pipeline having to error. An empty string (or `NA`) removes the label.
#'
#' @param data A data frame.
#' @param labels Named character vector (or named list) mapping column names
#'   to label strings.
#' @return `data` with updated label attributes.
#'
#' @examples
#' labelled <- set_column_labels(mtcars, c(mpg = "Miles per gallon"))
#' attr(labelled$mpg, "label")
#'
#' @export
set_column_labels <- function(data, labels = character()) {
  stopifnot(is.data.frame(data))

  labels <- unlist(as_label_list(labels))

  for (col in intersect(names(labels), colnames(data))) {
    lbl <- labels[[col]]
    attr(data[[col]], "label") <- if (is.na(lbl) || !nzchar(lbl)) NULL else lbl
  }

  data
}

#' Build the labeler expression
#' @param labels Named list of column labels
#' @return A language object
#' @noRd
make_labeler_expr <- function(labels = list()) {
  labels <- as_label_list(labels)

  if (length(labels) == 0) {
    return(quote(data))
  }

  bquote(blockr.extra::set_column_labels(data, .(unlist(labels))))
}

#' Normalize a labels argument to a named list of strings
#'
#' Accepts a named list or named character vector (constructor input, JSON
#' deserialization, or the JS state blob). Drops unnamed entries and anything
#' that isn't a length-1 atomic; keeps empty strings (they mean "remove the
#' label"). NAs normalize to "".
#' @noRd
as_label_list <- function(labels) {
  if (is.null(labels) || length(labels) == 0) {
    return(list())
  }

  labels <- as.list(labels)
  nms <- names(labels)

  keep <- nzchar(nms %||% character(length(labels))) &
    vapply(labels, function(x) is.atomic(x) && length(x) == 1L, logical(1))
  labels <- labels[keep]

  lapply(labels, function(x) {
    x <- as.character(x)
    if (is.na(x)) "" else x
  })
}

#' HTML dependencies for the labeler block's own JS + CSS
#' @noRd
labeler_block_dep <- memoise0(function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "labeler-block-js",
      version = utils::packageVersion("blockr.extra"),
      src = system.file("js", package = "blockr.extra"),
      script = "labeler-block.js"
    ),
    htmltools::htmlDependency(
      name = "labeler-block-css",
      version = utils::packageVersion("blockr.extra"),
      src = system.file("css", package = "blockr.extra"),
      stylesheet = "labeler-block.css"
    )
  )
})
