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
#' `{round(mean(data$mpg), 1)}` -- a bare `{colname}` does not resolve. In the
#' editor a reference renders as a chip showing its evaluated VALUE (the
#' expression is one click away); each chip is evaluated on its own, so a typo
#' in one reference marks that chip and leaves the rest of the text standing.
#'
#' Two things run on different clocks. Chip values refresh eagerly -- the
#' editor sends the expressions, the server evaluates each in the same
#' environment glue gets and pushes the values back -- which costs nothing in
#' the DAG. The document itself commits only on blur, Ctrl-Enter or the Apply
#' footer, never on a keystroke, so typing does not re-evaluate downstream
#' blocks.
#'
#' Braces the author types as literal text (Quarto attributes, shortcodes) are
#' escaped by the editor on the way out (doubled, glue's own escape), so the
#' stored markdown stays a valid glue template with no syntax for the author to
#' learn.
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

          # The live twin of the quoted env the expr builds below: the same
          # input names bound to the current data values, for the per-chip
          # preview. Inputs that are not ready bind nothing, so before data
          # arrives this env is empty and the preview reports dormant rather
          # than errors.
          r_env <- shiny::reactive({
            nms <- dot_args_names(...args)
            if (is.null(nms)) {
              nms <- names(...args)
            }
            vals <- lapply(shiny::isolate(names(...args)), function(nm) {
              arg_value(...args[[nm]])
            })
            names(vals) <- nms
            list2env(Filter(Negate(is.null), vals), parent = baseenv())
          })

          # Columns per input -> JS, for the "insert data field" chip menu.
          # Read off the data reactives (...args), keyed by dot name.
          shiny::observe({
            inputs <- lapply(shiny::isolate(names(...args)), function(nm) {
              as.list(colnames(arg_value(...args[[nm]])))
            })
            nm <- dot_args_names(...args)
            if (is.null(nm)) nm <- names(...args)
            names(inputs) <- nm
            session$sendCustomMessage(
              "prose-columns",
              list(id = session$ns("editor"), inputs = inputs)
            )
          })

          # Chip preview: the editor reports every reference expression it
          # holds (eagerly, on edit -- NOT the document commit), each is
          # evaluated on its own against r_env(), and the values travel back
          # in one message. A failure is local to its chip; nothing here
          # touches the block's expr or the DAG.
          shiny::observe({
            exprs <- unique(unlist(input$prose_exprs))
            env <- r_env()

            if (length(exprs) == 0L) {
              return()
            }

            dormant <- length(ls(env)) == 0L

            vals <- lapply(exprs, function(e) {
              if (dormant) {
                return(list(ok = NA))
              }
              tryCatch(
                {
                  v <- eval(parse(text = e)[[1L]], env)
                  # Bound the preview before formatting: a chip on a whole
                  # column must not stringify a million values to show 80
                  # characters.
                  n <- length(v)
                  v <- paste(format(utils::head(v, 20L), trim = TRUE),
                             collapse = ", ")
                  if (n > 20L || nchar(v) > 80L) {
                    v <- paste0(substr(v, 1L, 79L), "…")
                  }
                  list(ok = TRUE, value = v)
                },
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

          # UI -> state (explicit commit from JS: blur / Ctrl-Enter / Apply).
          # Guard so an external write is not clobbered by a stale commit
          # (static UI, no Pattern B needed).
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
                glue::glue(.(txt), .envir = .(env), .trim = FALSE),
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
