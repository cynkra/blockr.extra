#' Prose Block
#'
#' Text that can compute. The block holds markdown, edited as it reads (bold
#' as bold, no syntax to learn), and R inside it the way Quarto writes inline
#' code: `` `r nrow(data)` ``. Typing `` `r `` and a space in the editor opens
#' a small code field that suggests the inputs, their columns and a few common
#' functions; Enter computes it, and the text shows the value, marked with a
#' dotted underline. Pointing at a value shows its code, a click edits it.
#'
#' The markdown with its code is the block's setting, in its control. Its
#' result is the markdown with the values written in, which the next block
#' takes as text (a report, a slide). Because the code is Quarto's own inline
#' code, the same text runs in a Quarto document where the inputs are objects
#' of the same names.
#'
#' Inputs are bound by their input name: a link with `input = "data"` makes
#' `data` available. An expression that fails marks its value in the editor;
#' in the result it stops the block with the error.
#'
#' Values refresh eagerly as you type, outside the DAG. The text itself
#' commits on blur or Ctrl+Enter, never on a keystroke, so typing does not
#' recompute the blocks downstream.
#'
#' @param text Markdown string, with inline R as `` `r expr` ``.
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
#'         note = new_prose_block("## Iris\n\nThere are **`r nrow(data)`** rows.")
#'       ),
#'       links = links(from = "data", to = "note", input = "data")
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

  # An ...args element is a reactive in a live block server, but a bare value
  # under testServer (as.list.reactivevalues yields values, not callables).
  arg_value <- function(r) {
    if (is.function(r)) {
      tryCatch(r(), error = function(e) NULL)
    } else {
      r
    }
  }

  blockr.core::new_text_block(
    function(id, ...args) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_text <- shiny::reactiveVal(paste(text, collapse = "\n"))

          arg_names <- shiny::reactive(
            stats::setNames(names(...args), dot_args_names(...args))
          )

          # The inputs bound by input name to their current values, for the
          # value preview. Inputs that are not ready bind nothing, so before
          # data arrives the preview reports dormant rather than errors.
          r_data <- shiny::reactive({
            nms <- dot_args_names(...args)
            if (is.null(nms)) {
              nms <- names(...args)
            }
            vals <- lapply(shiny::isolate(names(...args)), function(nm) {
              arg_value(...args[[nm]])
            })
            names(vals) <- nms
            Filter(Negate(is.null), vals)
          })

          # Inputs and their columns -> JS, for the code field's suggestions.
          shiny::observe({
            inputs <- lapply(r_data(), function(x) {
              as.list(if (is.data.frame(x)) colnames(x) else names(x))
            })
            session$sendCustomMessage(
              "prose-columns",
              list(id = session$ns("editor"), inputs = inputs)
            )
          })

          # Value preview: the editor reports every inline expression it
          # holds (eagerly, on edit, not the document commit); each is
          # evaluated on its own and the values travel back in one message.
          # A failure is local to its value; nothing here touches the DAG.
          shiny::observe({
            exprs <- unique(unlist(input$prose_exprs))
            data <- r_data()

            if (length(exprs) == 0L) {
              return()
            }

            vals <- lapply(exprs, function(e) {
              if (!length(data)) {
                return(list(ok = NA))
              }
              tryCatch(
                list(ok = TRUE, value = inline_value(eval_inline(e, data), 80L)),
                error = function(err) {
                  list(ok = FALSE, value = conditionMessage(err))
                }
              )
            })

            session$sendCustomMessage(
              "prose-values",
              list(
                id = session$ns("editor"),
                values = stats::setNames(vals, exprs)
              )
            )
          })

          # UI -> state (explicit commit from JS: blur / Ctrl-Enter). Guard so
          # an external write is not clobbered by a stale commit.
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
                blockr.extra::inline_r(.(txt), list(..(data))),
                list(
                  txt = r_text(),
                  data = stats::setNames(
                    lapply(arg_names(), as_dot_call),
                    names(arg_names())
                  )
                ),
                splice = TRUE
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
        blockr.ui::controls_dep(),
        prose_block_dep(),
        shiny::div(
          id = shiny::NS(id, "editor"),
          class = "blockr-prose",
          `data-input-id` = shiny::NS(id, "text"),
          `data-exprs-id` = shiny::NS(id, "prose_exprs"),
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

#' Inline R in markdown
#'
#' Writes the value of every `` `r expr` `` in a markdown string into the
#' text, as knitr does for inline code: each expression is evaluated with the
#' objects in `data` in scope, and its value replaces the code. Code in fenced
#' blocks and other inline code are left alone.
#'
#' @param text A markdown string.
#' @param data A named list of objects the expressions can use.
#'
#' @return The markdown string with the values written in.
#'
#' @examples
#' inline_r("There are `r nrow(data)` cars.", list(data = mtcars))
#'
#' @export
inline_r <- function(text, data = list()) {

  text <- paste(text, collapse = "\n")
  if (!nzchar(text)) {
    return(text)
  }
  # split keeping empty lines, a trailing newline included
  lines <- regmatches(text, gregexpr("\n", text), invert = TRUE)[[1L]]
  fence <- cumsum(grepl("^\\s*(```|~~~)", lines)) %% 2L == 1L |
    grepl("^\\s*(```|~~~)", lines)

  pat <- "`r[ \t]+([^`]+)`"

  for (i in which(!fence)) {
    m <- gregexpr(pat, lines[i], perl = TRUE)[[1L]]
    if (m[1L] == -1L) next
    codes <- regmatches(lines[i], list(m))[[1L]]
    vals <- vapply(
      codes,
      function(code) {
        inline_value(eval_inline(sub(pat, "\\1", code, perl = TRUE), data))
      },
      character(1L)
    )
    regmatches(lines[i], list(m)) <- list(vals)
  }

  paste(lines, collapse = "\n")
}

# One inline expression, evaluated with the inputs in scope. Functions come
# from the search path, as they do in a Quarto document.
eval_inline <- function(expr, data) {
  env <- list2env(as.list(data), parent = globalenv())
  eval(parse(text = expr, keep.source = FALSE)[[1L]], env)
}

# A value as text in a sentence: numbers as format() writes them, a vector
# as a comma-separated list. `max` shortens it for the editor's preview.
inline_value <- function(x, max = NULL) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  n <- length(x)
  out <- paste(format(utils::head(x, 20L), trim = TRUE, big.mark = ""), collapse = ", ")
  if (!is.null(max) && (n > 20L || nchar(out) > max)) {
    out <- paste0(substr(out, 1L, max - 1L), "…")
  }
  out
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
