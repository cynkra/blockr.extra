#' Prose Block
#'
#' A WYSIWYG-first text block: a rich-text editor (no markdown syntax to learn)
#' backed by canonical markdown. The editor lives in the block control; the
#' rendered, glue-evaluated markdown is the block result. A collapsible
#' "Markdown source" field exposes the raw markdown, two-way synced with the
#' editor. The `text` parameter is externally controllable, so an assistant
#' (blockr.ai) can write the note as markdown at runtime.
#'
#' Like [blockr.core::new_glue_block()], the text is evaluated with
#' [glue::glue()] against the input data, which is bound by its input name (e.g.
#' `data`). Reference data with `{nrow(data)}`, `{data$colname}` or
#' `{round(mean(data$mpg), 1)}` -- a bare `{colname}` does not resolve. Such
#' references render as editable chips in the WYSIWYG editor.
#'
#' @param text Markdown string, evaluated with [glue::glue()].
#' @param ... Forwarded to [blockr.core::new_block()].
#'
#' @return A `prose_block` (also a `text_block`).
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_board(
#'       blocks = list(
#'         data = new_dataset_block("iris", "datasets"),
#'         note = new_prose_block("## Iris\n\nThere are **{nrow(data)}** rows.")
#'       ),
#'       links = links(from = "data", to = "note")
#'     )
#'   )
#' }
#'
#' @export
new_prose_block <- function(text = character(), ...) {

  # blockr.core does not export these helpers; replicate locally (the same
  # pattern function_var_block.R already uses for dot_args_names).
  dot_args_names <- function(x) {
    res <- names(x)
    unnamed <- grepl("^[1-9][0-9]*$", res)
    if (all(unnamed)) {
      return(NULL)
    }
    if (any(unnamed)) {
      return(replace(res, unnamed, ""))
    }
    res
  }
  as_dot_call <- function(x) call(".", as.name(x))

  blockr.core::new_text_block(
    function(id, ...args) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_text <- shiny::reactiveVal(paste(text, collapse = "\n"))

          arg_names <- shiny::reactive(
            stats::setNames(names(...args), dot_args_names(...args))
          )

          # Columns per input -> JS, for the "insert data field" chip menu.
          # Read off the data reactives (...args), keyed by dot name.
          shiny::observe({
            inputs <- lapply(...args, function(r) {
              tryCatch(as.list(colnames(r())), error = function(e) list())
            })
            nm <- dot_args_names(...args)
            if (is.null(nm)) nm <- names(...args)
            names(inputs) <- nm
            session$sendCustomMessage(
              "prose-columns",
              list(id = session$ns("editor"), inputs = inputs)
            )
          })

          # UI -> state (debounced commit from JS). Guard so an external write
          # is not clobbered by a stale commit (static UI, no Pattern B needed).
          shiny::observeEvent(input$text, {
            if (!identical(input$text, shiny::isolate(r_text()))) {
              r_text(input$text)
            }
          })

          # State -> UI: external/AI writes pushed to JS (Pattern A reverse
          # sync). The self-write guard lives in JS (prose-set does not commit).
          shiny::observeEvent(r_text(), {
            session$sendCustomMessage(
              "prose-set",
              list(id = session$ns("editor"), markdown = r_text())
            )
          }, ignoreInit = TRUE)

          list(
            expr = shiny::reactive(
              bquote(
                glue::glue(.(txt), .envir = .(env)),
                list(
                  txt = r_text(),
                  env = bquote(
                    list2env(list(..(data)), parent = baseenv()),
                    list(data = lapply(arg_names(), as_dot_call)),
                    splice = TRUE
                  )
                )
              )
            ),
            state = list(
              text = r_text
            )
          )
        }
      )
    },
    function(id) {
      shiny::tagList(
        prose_block_dep(),
        shiny::div(
          id = shiny::NS(id, "editor"),
          class = "blockr-prose",
          `data-input-id` = shiny::NS(id, "text"),
          `data-initial` = paste(text, collapse = "\n")
        )
      )
    },
    expr_type = "bquoted",
    class = "prose_block",
    external_ctrl = "text",
    ...
  )
}

#' HTML dependency for the prose block JS/CSS
#' @keywords internal
prose_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-prose-js",
      version = utils::packageVersion("blockr.extra"),
      src = system.file("js", package = "blockr.extra"),
      script = "prose-block.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-prose-css",
      version = utils::packageVersion("blockr.extra"),
      src = system.file("css", package = "blockr.extra"),
      stylesheet = "prose-block.css"
    )
  )
}
