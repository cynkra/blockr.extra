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
  # Use a null device to suppress any plot output during initial eval
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- eval(expr, env)

  # If result is a ggplot, return it directly (no need for recordedplot)
  if (inherits(result, "ggplot")) {
    return(result)
  }

  # If result is NULL, a base R plot might have been created
  # Use evaluate to capture it properly
  if (is.null(result)) {
    expr_text <- paste(deparse(expr), collapse = "\n")

    # Create fresh environment with same data
    env2 <- new.env(parent = parent.env(env))
    for (nm in ls(env, all.names = TRUE)) {
      env2[[nm]] <- env[[nm]]
    }

    res <- evaluate::evaluate(expr_text, env2, stop_on_error = 1L)

    # Check for recorded plot - return the last one since iterative plots
    # (e.g., plot() followed by text() calls) produce multiple snapshots
    recorded_plots <- Filter(function(r) inherits(r, "recordedplot"), res)
    if (length(recorded_plots) > 0) {
      return(recorded_plots[[length(recorded_plots)]])
    }
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
