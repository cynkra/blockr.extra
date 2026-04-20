#' Latest-Wins Merge Block
#'
#' Variadic transform block that forwards the value of whichever input most
#' recently changed. Implements the FRP `merge` / `switchLatest` operator:
#' one-of semantics across `n` inputs, where "active" = "last to fire after
#' mount".
#'
#' Typical use: bridge multiple drill-down charts into a single downstream
#' patient-profile / detail block. Each chart emits a filtered data frame on
#' click; `new_latest_block()` picks the one whose click was most recent and
#' passes it through unchanged.
#'
#' @section Reproducibility note:
#' The block's output depends on the order of user interactions during the
#' session, so the board as a static artefact is not fully deterministic.
#' The most recently active input name is persisted via the block's `state`
#' (`last_active`) so restored sessions pick up where they left off.
#'
#' @section Initial state:
#' Before any input fires, the block emits the first input's value (by name
#' order within `...args`). `ignoreInit = TRUE` on the per-input observers
#' prevents the topo-sort mount order from being interpreted as a "click".
#'
#' @param last_active Character(1). Name of the input to treat as active on
#'   first load. Usually left `NULL` (falls back to the first input) — set
#'   automatically by the serialization layer when restoring a session.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A variadic transform block of class `latest_block`.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_board(
#'       blocks = list(
#'         a = new_dataset_block(dataset = "iris"),
#'         b = new_dataset_block(dataset = "mtcars"),
#'         pick = new_latest_block()
#'       ),
#'       links = links(
#'         from = c("a", "b"), to = c("pick", "pick"),
#'         input = c("1", "2")
#'       )
#'     )
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observe observeEvent
#'   req textOutput renderText NS tagList div strong isolate
#' @export
new_latest_block <- function(last_active = NULL, ...) {

  blockr.core::new_transform_block(
    server = function(id, ...args) {
      moduleServer(id, function(input, output, session) {

        r_active <- reactiveVal(last_active)

        # blockr wires `...args` lazily: at mount time `names(...args)` is
        # empty, and keys are added one per upstream link as those blocks
        # emit their first result. So we can't install per-key observers in
        # the moduleServer body — we sync the observer set from within a
        # tracker on `names(...args)` that re-runs whenever a new key
        # appears. Each observer uses ignoreInit = TRUE so the topo-sort
        # mount reads don't count as user clicks; the first firing sets
        # r_active to the name of whichever input fired.
        obs_store <- list()
        observe({
          nms <- names(...args)
          isolate({
            for (nm in setdiff(nms, names(obs_store))) {
              local({
                key <- nm
                obs_store[[key]] <<- observeEvent(
                  ...args[[key]],
                  r_active(key),
                  ignoreInit = TRUE,
                  ignoreNULL = TRUE
                )
              })
            }
            if (is.null(r_active()) && length(nms)) {
              r_active(nms[[1L]])
            }
          })
        })

        output$active_source <- renderText({
          a <- r_active()
          if (is.null(a) || !nzchar(a)) "Showing: (no source)"
          else paste0("Showing: ", a)
        })

        list(
          expr = reactive({
            a <- r_active()
            req(a, nzchar(a))
            # `.(a)` is replaced with `as.name(a)` by the bquoted eval pass,
            # which resolves to the named input's value in the eval env.
            call(".", as.name(a))
          }),
          state = list(
            last_active = r_active
          )
        )
      })
    },
    ui = function(id) {
      ns <- NS(id)
      tagList(
        div(
          class = "latest-block",
          strong(textOutput(ns("active_source"), inline = TRUE))
        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    expr_type = "bquoted",
    class = "latest_block",
    ...
  )
}
