#' Evaluate expression and capture any output type (including base R plots)
#'
#' Uses evaluate::evaluate() to capture base R plots as recordedplot objects.
#' Returns the result value, or a recordedplot if one was produced.
#'
#' @param expr Expression to evaluate
#' @param env Environment for evaluation
#' @return The result (could be any R object, including recordedplot)
#' @noRd
eval_with_plot_capture <- function(expr, env) {
  expr_text <- paste(deparse(expr), collapse = "\n")

  # Create environment with access to all attached packages (stats, graphics,
  # etc.). all.names = TRUE is load-bearing: variadic inputs are bound under
  # dot-prefixed reference symbols (.arg1, .arg2, ... for unnamed DAG-UI slots),
  # and the default as.list() drops names starting with "." — which would strip
  # those inputs out of the eval environment.
  eval_env <- list2env(as.list(env, all.names = TRUE), parent = .GlobalEnv)

  # Use evaluate to run code and capture any plots
  res <- evaluate::evaluate(
    expr_text,
    eval_env,
    stop_on_error = 1L,
    output_handler = evaluate::new_output_handler(value = identity)
  )

  result <- NULL
  recorded_plot <- NULL

  for (item in res) {
    if (inherits(item, "recordedplot")) {
      # Keep the last plot (in case of multiple snapshots)
      recorded_plot <- item
    } else if (inherits(item, "error")) {
      stop(conditionMessage(item))
    } else if (!inherits(item, c("source", "message", "warning"))) {
      # This is likely the return value
      result <- item
    }
  }

  # Priority: ggplot result > recorded plot > other result
  if (inherits(result, "ggplot")) {
    return(result)
  }

  if (!is.null(recorded_plot)) {
    return(recorded_plot)
  }

  result
}

#' Is `x` an HTML-renderable object?
#'
#' TRUE for objects that carry their own HTML via the `htmltools::as.tags()`
#' contract: native HTML (`shiny.tag`, `html`, ...), htmlwidgets (plotly,
#' leaflet, DT, ...), gt tables, and anything that registers a dedicated
#' `as.tags` method (e.g. composer's `composed_table` — see blockr.sandbox).
#'
#' Deliberately a tight allow-list, not "does `as.tags()` not error": `as.tags`
#' is eager (it turns a bare string or list into text/tag nodes), so we exclude
#' base implicit classes and only accept a *dedicated* `as.tags` method. Keeps
#' plain strings, numbers, lists and data frames off this branch so they reach
#' their own renderers (DataTable / preformatted text).
#'
#' @param x Any R object.
#' @return Single logical.
#' @noRd
is_html_renderable <- function(x) {
  if (inherits(x, c("shiny.tag", "shiny.tag.list", "html", "htmlwidget"))) {
    return(TRUE)
  }
  # A dedicated as.tags method on a non-base class (gt_tbl, composed_table, ...).
  # Look the method up in htmltools' namespace (the generic's home): as.tags is
  # imported-not-attached here, so a bare-name getS3method() would miss it. The
  # base-class exclusion drops htmltools' own eager as.tags.character/.list/...
  # so plain strings/lists/data frames don't get mistaken for HTML.
  base_classes <- c(
    "list", "character", "numeric", "integer", "double", "logical", "complex",
    "factor", "data.frame", "function", "NULL", "environment", "name", "call"
  )
  cls <- setdiff(class(x), base_classes)
  any(vapply(
    cls,
    function(cl) {
      # Registered method (installed packages: gt, or a load_all'd blockr.sandbox)
      !is.null(utils::getS3method(
        "as.tags", cl, optional = TRUE, envir = asNamespace("htmltools")
      )) ||
        # Method sourced into the global env / search path. blockr.sandbox is
        # deployed as an app bundle and `source()`s its composer methods into
        # GlobalEnv (see its app.R); UseMethod dispatches to those via the search
        # path, so mirror that lookup here or the object misses this branch.
        exists(
          paste0("as.tags.", cl),
          envir = globalenv(), mode = "function", inherits = TRUE
        )
    },
    logical(1)
  ))
}

#' Render any R object dynamically based on its type
#'
#' Detects the type of result and renders appropriately:
#' - HTML-renderable (gt, htmlwidgets, composer tables, raw tags): as.tags() HTML
#' - ggplot: plotOutput with renderPlot
#' - recordedplot: plotOutput with evaluate::replay
#' - data.frame: DataTable
#' - other: print() as preformatted text
#'
#' @param result The R object to render
#' @param block The block object (for dt_datatable options)
#' @param session Shiny session
#' @return A shiny.render.function (renderUI)
#' @noRd
#' House styling for a gt table, applied at RENDER time
#'
#' Three properties, and all of them are about the app's chrome rather than
#' about the table. gt sizes itself to its content, centres itself
#' (`margin-left: auto`) and ships its own font stack (`system-ui, 'Segoe UI',
#' Roboto, ...`), so a gt table lands in a blockr panel looking like a visitor:
#' a narrow slab adrift in the middle of a card, set in a different typeface
#' from everything around it. A panel is a box the user sized on purpose, so
#' the table takes the width it was given; `"inherit"` hands the type back to
#' the panel's CSS, which is the board's theme.
#'
#' Width does the work that alignment only half did: at `pct(100)` the table
#' fills the card and there is no slack left to align within. `table.align`
#' stays anyway, for the case where a caption or a narrow column keeps the
#' table under full width.
#'
#' DELIBERATELY NOT IN THE BLOCK'S SCRIPT. A code block's script is the code it
#' EXPORTS, so `gt::tab_options()` written there would travel into the reader's
#' document and impose our house style on theirs. Same reason `as_gt()` is
#' here: how a thing draws in our app is our business, and the exported call
#' should stay the call a statistician would have typed.
#'
#' The cost, named: this runs on the finished object, so a script that sets
#' width, alignment or a font on purpose is overridden. Those three are the
#' only properties taken; colours, borders, spanners, footnotes and the rest
#' of gt are left exactly as the author left them.
#'
#' @param x A `gt_tbl`.
#' @return The `gt_tbl`, restyled; unchanged if gt is unavailable.
#' @noRd
gt_house_style <- function(x) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    return(x)
  }
  tryCatch(
    gt::tab_options(
      x,
      table.width = gt::pct(100),
      table.align = "left",
      table.font.names = "inherit",
      table.font.size = "inherit"
    ),
    error = function(e) x
  )
}

render_dynamic_output <- function(result, block, session) {
  # A NULL RESULT PAINTS NOTHING. Everything below ends at `print()` as
  # preformatted text for an object no branch claims, and printing NULL puts
  # the word NULL on the card.
  #
  # It is not a result the user wrote. blockr.core hands one down whenever the
  # block is momentarily outside the eval set -- `res()` returns NULL early
  # while `block_ready()`, which gates the render observer, does not test
  # `needed()` (blockr.core R/block-server.R). A view switch lands a block's
  # visibility across several flushes, so a block already on screen takes that
  # round trip and its output is rendered from the NULL. On a slow connection
  # the two paints arrive as two messages and the table is preceded by the
  # word NULL for a fraction of a second.
  #
  # A block that genuinely returns NULL paints nothing too, which is the same
  # thing the table block does with it (an empty frame, drawn empty).
  if (is.null(result)) {
    return(shiny::renderUI(NULL))
  }

  # A GTSUMMARY TABLE IS A DESCRIPTION OF A TABLE, NOT A RENDERED ONE. Its
  # HTML comes from gt, reached through `gtsummary::as_gt()`, so it has no
  # `as.tags` method of its own and would otherwise fall through to
  # `print()` as preformatted text.
  #
  # Converted HERE and not in the block's script on purpose: the script a
  # code block holds is also the code it EXPORTS, so a wrapper written only
  # to satisfy this renderer would travel into the exported document with
  # it. `gtsummary::tbl_summary(data, by = AREA)` is the line a statistician
  # writes and the line the report should carry; making it draw is this
  # function's job, not the author's.
  if (inherits(result, "gtsummary") &&
        requireNamespace("gtsummary", quietly = TRUE)) {
    result <- tryCatch(gtsummary::as_gt(result), error = function(e) result)
  }

  # Any gt table, however it got here -- a gtsummary above, a `gt::gt()` in a
  # code block, a sibling package's builder.
  if (inherits(result, "gt_tbl")) {
    result <- gt_house_style(result)
  }

  # Data frames follow the board's chosen tabular display, so a function block
  # previews its result the same way the data and transform blocks around it
  # do. Only the HTML table can be honored here, and the reason is the
  # container: this function is the render half of a FIXED `uiOutput` (see
  # block_ui.function_block), while a blockr.core `tabular_display` also picks
  # its own container -- `minimal_display` pairs a renderText with a
  # verbatimTextOutput, and shipping that text into a uiOutput binding renders
  # nothing. blockr.ui's display is the one whose renderer is already a
  # renderUI, so it drops straight in; every other display falls back to DT
  # below rather than being routed through a container it did not ask for.
  if (inherits(result, "data.frame") &&
      inherits(blockr.core::tabular_display(), "html_table_display")) {
    return(blockr.ui::html_table_result(result, block, session))
  }
  shiny::renderUI({
    if (is_html_renderable(result)) {
      # Ask the object for its HTML (gt, htmlwidgets, composer composed_table,
      # ...). Fall back to text if the contract unexpectedly errors.
      tryCatch(
        htmltools::as.tags(result),
        error = function(e) {
          shiny::pre(
            style = "background: #f8f9fa; padding: 10px; border-radius: 4px; overflow-x: auto;",
            paste(utils::capture.output(print(result)), collapse = "\n")
          )
        }
      )
    } else if (inherits(result, "ggplot")) {
      output_id <- "plot_output"
      session$output[[output_id]] <- shiny::renderPlot({
        print(result)
      }, bg = "transparent")
      shiny::plotOutput(session$ns(output_id))
    } else if (inherits(result, "recordedplot")) {
      output_id <- "plot_output"
      session$output[[output_id]] <- shiny::renderPlot({
        evaluate::replay(result)
      })
      shiny::plotOutput(session$ns(output_id))
    } else if (inherits(result, "data.frame")) {
      # html_table_display case handled above via blockr.ui::html_table_result()
      dt_datatable(result, block, session)
    } else {
      # Fallback: print method as preformatted text
      shiny::pre(
        style = "background: #f8f9fa; padding: 10px; border-radius: 4px; overflow-x: auto;",
        paste(utils::capture.output(print(result)), collapse = "\n")
      )
    }
  })
}
