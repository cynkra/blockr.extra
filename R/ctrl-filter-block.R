#' Cohort filter sender block
#'
#' Reads the drilled output of an upstream table, chart or tile block and
#' pushes the drilled-to condition into a **value filter block elsewhere on the
#' board**, over the board's external-control channel ([ctrl_send()]). Click a
#' level row in a summary and the filter narrows to it; re-click to clear.
#'
#' This is the packaged form of the sender that was prototyped as a
#' [new_function_block()] calling [ctrl_send()] by hand. Prefer this block when
#' you need more than one or two senders: the claim logic
#' ([drill_claim_columns()]) lives in the package rather than in a deparsed
#' function string copied once per sender, the configuration is real block
#' arguments (so it is AI-configurable and restores with the board), and there
#' is no code editor to mount.
#'
#' @section Output vs receipt:
#' The block's **output** is its input, untouched -- the drilled subset passes
#' straight through, so the block can still be linked downstream. The
#' **receipt** ("Sent to Cohort: SEX = F") renders in the block's control
#' section via [ctrl_receipt()].
#'
#' The receipt is built from the conditions this block just sent, and nothing
#' else: the sender never reads the target block back. There is no back-edge,
#' by design (see [ctrl_receipt()]). It is therefore written in the past tense
#' -- it reports what this block pushed, which stays true even after another
#' sender replaces the claim on the same target ([ctrl_send()] is
#' last-write-wins, and [ctrl_clear()] only ever clears your own claim).
#'
#' @section Unconfigured:
#' With no `target`, the block passes data through and sends nothing. It does
#' not error.
#'
#' @section Upstream: table, chart or tile:
#' A **table / summary** drill carries the ARD identity (`.variable`,
#' `.group<k>`), and the block reads the claim off it with no further
#' configuration -- leave `columns` empty.
#'
#' A **chart or tile** drill does not: the chart coerces its input with
#' `as_plain_df()`, which drops those columns, so its drilled output is a plain
#' row subset with nothing in it naming the drilled column. For those, set
#' `columns` to the source column(s) a click may claim -- typically the chart's
#' own `drill` column, e.g. `columns = "SEX"`. See [drill_claim_columns()].
#'
#' @param target Character(1). Block id of the value filter block to control,
#'   e.g. `"cohort_filter"`. Empty means "not configured": pass through, send
#'   nothing.
#' @param table Character(1). Name of the table in the target's `dm` that the
#'   conditions apply to, e.g. `"adsl"`.
#' @param columns Character vector of source columns a click may claim, for
#'   chart / tile upstreams whose drilled output carries no ARD identity. Empty
#'   (the default) reads the ARD identity instead -- the table / summary case.
#' @param label Character(1) or `NULL`. Name of the target as shown on the
#'   receipt. Defaults to `target`.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @examples
#' if (interactive()) {
#'   # See blockr.pharma/dev/composer-cohort-drill-two-senders.R for a board
#'   # wiring two of these into a single value filter.
#'   new_ctrl_filter_block(target = "cohort_filter", table = "adsl")
#' }
#'
#' @export
new_ctrl_filter_block <- function(target = "", table = "", columns = character(),
                                  label = NULL, ...) {

  stopifnot(
    is.character(target), length(target) == 1L,
    is.character(table), length(table) == 1L,
    is.character(columns),
    is.null(label) || (is.character(label) && length(label) == 1L)
  )

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_target <- shiny::reactiveVal(target)
          r_table <- shiny::reactiveVal(table)
          r_columns <- shiny::reactiveVal(columns)
          r_label <- shiny::reactiveVal(label)

          shiny::observeEvent(input$target, r_target(trimws(input$target)))
          shiny::observeEvent(input$table, r_table(trimws(input$table)))
          shiny::observeEvent(input$columns, r_columns(split_columns(input$columns)))

          # What the drill claims, if anything. Empty list = no claim: the
          # upstream is undrilled, or the drill resolves no dimension to a
          # single value.
          cols <- shiny::reactive({
            if (!nzchar(r_table())) {
              return(list())
            }
            drill_claim_columns(
              data(),
              table = r_table(),
              columns = r_columns()
            )
          })

          # Push it. A claim sends; no claim clears -- but ctrl_clear() only
          # acts if this block still owns the target's last claim, so an
          # un-drill never clobbers a sibling sender's cohort.
          shiny::observe({
            tgt <- r_target()

            if (!nzchar(tgt)) {
              return()
            }

            payload <- list(columns = cols())

            if (length(cols())) {
              ctrl_send(tgt, state = payload)
            } else {
              ctrl_clear(tgt, state = payload)
            }
          })

          output$receipt <- shiny::renderUI({
            tgt <- r_target()

            if (!nzchar(tgt)) {
              return(
                ctrl_receipt(
                  list(),
                  hint = "Set a target block to send the drilled cohort to."
                )
              )
            }

            ctrl_receipt(
              cols(),
              target = r_label() %||% tgt,
              hint = if (length(cols())) {
                "Re-click the row in the summary to clear."
              } else {
                "Click a level row in the summary to filter the cohort."
              }
            )
          })

          list(
            # The drilled subset passes straight through: this block sends a
            # control message, it does not transform data.
            expr = shiny::reactive(quote(data)),
            state = list(
              target = r_target,
              table = r_table,
              columns = r_columns,
              label = r_label
            )
          )
        }
      )
    },
    function(id) {
      ns <- shiny::NS(id)

      shiny::tagList(
        shiny::div(
          class = "ctrl-filter-block",
          shiny::div(
            class = "ctrl-filter-config",
            shiny::div(
              class = "block-input-wrapper",
              shiny::textInput(
                ns("target"),
                "Target block",
                value = target,
                placeholder = "e.g. cohort_filter"
              )
            ),
            shiny::div(
              class = "block-input-wrapper",
              shiny::textInput(
                ns("table"),
                "Table",
                value = table,
                placeholder = "e.g. adsl"
              )
            ),
            # Only needed upstream of a chart / tile, whose drilled output
            # carries no ARD identity to read the claim off.
            shiny::div(
              class = "block-input-wrapper",
              shiny::textInput(
                ns("columns"),
                "Claim columns (chart / tile only)",
                value = paste(columns, collapse = ", "),
                placeholder = "e.g. SEX, ARM — leave empty for tables"
              )
            )
          ),
          shiny::uiOutput(ns("receipt"))
        )
      )
    },
    class = "ctrl_filter_block",
    allow_empty_state = c("target", "table", "columns", "label"),
    ...
  )
}

# "SEX, ARM" -> c("SEX", "ARM"); "" -> character()
split_columns <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  out <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  out[nzchar(out)]
}
