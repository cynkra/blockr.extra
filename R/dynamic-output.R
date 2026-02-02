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

  # Create environment with access to all attached packages (stats, graphics, etc.)
  eval_env <- list2env(as.list(env), parent = .GlobalEnv)

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

#' Render any R object dynamically based on its type
#'
#' Detects the type of result and renders appropriately:
#' - gt_tbl: GT HTML
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
  shiny::renderUI({
    if (inherits(result, "gt_tbl")) {
      shiny::HTML(gt::as_raw_html(result))
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
