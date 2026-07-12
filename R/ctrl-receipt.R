#' Receipt for a cross-block control send
#'
#' Builds the status card a sender block returns as its output, so a
#' [new_function_block()] driving another block via [ctrl_send()] shows a
#' readable summary instead of a debug data frame. Function block outputs
#' render HTML when the result is html-renderable, so returning this as the
#' last expression of the `fn` is all it takes.
#'
#' @section It never reads the target:
#' `ctrl_receipt()` is a pure function of its arguments: it formats `cols`,
#' which the caller has just computed, and knows nothing else. It does not
#' read the target block's state, the board, or any reactive -- there is no
#' back-edge from target to sender, and there must not be one: a function
#' block's `fn` re-runs as a whole, so a reactive read of the target would
#' re-trigger the [ctrl_send()] in the same body, which re-triggers any
#' sibling sender, forever.
#'
#' That is why the card is written in the past tense ("Sent to Cohort"). It
#' reports what this block pushed, which stays true whatever happens to the
#' target afterwards -- including a later sender replacing this claim
#' (`ctrl_send()` is last-write-wins). What the target currently *holds* is
#' the target's own business to display.
#'
#' @param cols Columns pushed to the target, in the shape a value filter
#'   takes: a list of `list(name=, table=, mode=, values=)` entries. Empty
#'   (or `NULL`) renders the idle state.
#' @param target Character(1). Human-readable name of the block written to,
#'   e.g. `"Cohort"`. Shown in the heading.
#' @param hint Character(1) or `NULL`. Secondary line: how to undo (active
#'   state) or how to start (idle state).
#'
#' @return An [htmltools::tag()], carrying its own stylesheet dependency.
#'
#' @examples
#' # Inside a sender function block's fn (always qualify: fns are deparsed):
#' # function(data) {
#' #   cols <- list(list(name = "SEX", table = "adsl",
#' #                     mode = "multi", values = "F"))
#' #   blockr.extra::ctrl_send("cohort_filter", state = list(columns = cols))
#' #   blockr.extra::ctrl_receipt(cols, "Cohort")
#' # }
#'
#' @export
ctrl_receipt <- function(cols = list(), target = NULL, hint = NULL) {

  stopifnot(
    is.null(cols) || is.list(cols),
    is.null(target) || (is.character(target) && length(target) == 1L),
    is.null(hint) || (is.character(hint) && length(hint) == 1L)
  )

  if (!length(cols)) {
    return(
      htmltools::div(
        class = "ctrl-receipt ctrl-receipt--idle",
        htmltools::div(class = "ctrl-receipt-head", "Nothing sent"),
        receipt_hint(hint %||% "Click a level row in the summary."),
        ctrl_receipt_dep()
      )
    )
  }

  chips <- lapply(cols, function(col) {
    htmltools::div(
      class = "ctrl-receipt-chip",
      htmltools::span(class = "ctrl-receipt-name", as.character(col$name)),
      htmltools::span(class = "ctrl-receipt-op", "="),
      htmltools::span(
        class = "ctrl-receipt-value",
        paste(as.character(col$values), collapse = ", ")
      )
    )
  })

  htmltools::div(
    class = "ctrl-receipt",
    htmltools::div(
      class = "ctrl-receipt-head",
      if (is.null(target)) {
        "Sent"
      } else {
        htmltools::tagList(
          "Sent to ",
          htmltools::span(class = "ctrl-receipt-target", target)
        )
      }
    ),
    htmltools::div(class = "ctrl-receipt-chips", chips),
    receipt_hint(hint),
    ctrl_receipt_dep()
  )
}

receipt_hint <- function(hint) {
  if (is.null(hint)) {
    return(NULL)
  }
  htmltools::div(class = "ctrl-receipt-hint", hint)
}

ctrl_receipt_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-ctrl-receipt",
    version = as.character(utils::packageVersion("blockr.extra")),
    src = system.file("css", package = "blockr.extra"),
    stylesheet = "ctrl-receipt.css"
  )
}
