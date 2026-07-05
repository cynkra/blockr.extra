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
render_dynamic_output <- function(result, block, session) {
  # Data frames with the preview option on render via the shared blockr.ui
  # table preview (a complete renderUI with its own sort/page handling).
  if (inherits(result, "data.frame") &&
      isTRUE(getOption("blockr.html_table_preview", FALSE))) {
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
      # preview-option case handled above via blockr.ui::html_table_result()
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
