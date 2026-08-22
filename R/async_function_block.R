#' Async Function Block
#'
#' A block that wraps a user-defined R function and executes it asynchronously
#' using mirai + ExtendedTask. Unlike `new_function_block()`, this block requires
#' manual execution via a "Run" button and supports cancellation.
#'
#' @section Approach:
#' The function must have `data` as its first argument (the input data frame).
#' Additional arguments are introspected and UI is generated based on defaults:
#' - Character vector with multiple elements -> selectInput (dropdown)
#' - Single numeric value -> numericInput
#' - Single logical value -> checkboxInput
#' - Single character value -> textInput
#'
#' @section Output Types:
#' Output type is auto-detected from the function's return value (in priority order):
#' - `gt_tbl`: renders as GT HTML table
#' - `ggplot`: renders with plotOutput/renderPlot
#' - `recordedplot` (base R plot): renders with evaluate::replay() in renderPlot
#' - `data.frame`/`tibble`: renders as interactive DataTable
#' - Any other object: shows print() output as preformatted text
#'
#' @section Async Execution:
#' - Click "Run" to execute the function in a background mirai daemon
#' - Click "Cancel" to stop a running computation
#' - Status indicators show initial/running/completed/error states
#' - Requires `mirai::daemons()` to be set up before use
#'
#' @param fn Character string containing R function code that transforms a data
#'   frame. The function must have `data` as first argument. Default values of
#'   other arguments determine the UI widgets.
#' @param ... Additional arguments passed to new_block
#'
#' @examples
#' \dontrun{
#' # Setup daemons first
#' mirai::daemons(2)
#' shiny::onStop(function() mirai::daemons(0))
#'
#' # Create an async block with simulated slow computation
#' blk <- new_async_function_block(
#'   fn = "function(data, n = 6L) {
#'     Sys.sleep(2)  # Simulate slow computation
#'     utils::head(data, n)
#'   }"
#' )
#' }
#'
#' @importFrom blockr.core block_eval block_output block_ui
#' @export
new_async_function_block <- function(
    fn = "function(data, n = 6L) { Sys.sleep(2); utils::head(data, n) }",
    ...) {

  fn_text <- fn
  fn <- parse_function_code(fn_text)
  validate_function_args(fn, "data", "Function must have 'data' as its first argument")

  blockr.core::new_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Setup common server infrastructure (params, validation).
          base <- setup_function_block_server(
            input = input,
            output = output,
            session = session,
            fn_text = fn_text,
            required_args = "data",
            skip_args = "data",
            error_message = "Function must have 'data' as its first argument"
          )

          # The shared Blockr.Code editor (AI-diff, dirty footer, run-on-commit).
          # Editing applies the code to base$r_fn(); execution is still manual
          # via the async Run button below.
          setup_code_editor_server(
            input, output, session, base,
            cols = shiny::reactive(
              tryCatch(names(data()), error = function(e) character())
            )
          )

          # === ASYNC-SPECIFIC ===

          # Store mirai handles for cancellation
          mirais <- list()

          # Create ExtendedTask for async execution
          run_task <- shiny::ExtendedTask$new(function(fn, data, params) {
            mirais[["task"]] <<- mirai::mirai(
              {
                .fn <- fn
                do.call(.fn, c(list(data = data), params))
              },
              fn = fn,
              data = data,
              params = params
            )
          })

          # Bind task button for automatic state management
          bslib::bind_task_button(run_task, "run")

          # Run on button click
          shiny::observeEvent(input$run, {
            current_fn <- base$r_fn()
            shiny::req(current_fn, data())
            params <- base$get_param_values()

            run_task$invoke(
              fn = current_fn,
              data = data(),
              params = params
            )
          })

          # Cancel button
          shiny::observeEvent(input$cancel, {
            if (!is.null(mirais[["task"]])) {
              mirai::stop_mirai(mirais[["task"]])
            }
          })

          # Update cancel button state based on task status
          shiny::observe({
            status <- run_task$status()
            shiny::updateActionButton(
              session,
              "cancel",
              disabled = status != "running"
            )
          }) |>
            shiny::bindEvent(run_task$status())

          # Result - return a bquote expression that blockr.core will evaluate
          # This pattern matches blockr.model's approach
          # Note: ExtendedTask$status() returns "initial", "running", "success", or "error"
          result <- shiny::reactive({
            status <- run_task$status()
            if (status == "success") {
              tryCatch(
                {
                  res <- run_task$result()
                  # Return bquote that adds status attributes to the result
                  bquote(structure(.(res), .async_status = "success"))
                },
                error = function(e) {
                  bquote(structure(list(), .async_status = "error", .async_error = .(e$message)))
                }
              )
            } else if (status == "error") {
              tryCatch(
                run_task$result(),
                error = function(e) {
                  bquote(structure(list(), .async_status = "error", .async_error = .(e$message)))
                }
              )
            } else {
              # initial or running - return expression that evaluates to NULL with status
              bquote(structure(list(), .async_status = .(status)))
            }
          })

          list(
            expr = result,
            # `fn` must be the reactiveVal itself (not a reactive wrapper) so
            # external_ctrl="fn" / the per-block AI can write to it.
            state = list(
              fn = base$r_fn_text
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      async_function_block_ui(
        ns = ns,
        fn_text = fn_text
      )
    },
    class = "async_function_block",
    allow_empty_state = TRUE,
    # Now that the editor is the shared Blockr.Code surface, the per-block AI
    # (blockr.ai) can author `fn` here too, landing as an inline diff.
    external_ctrl = "fn",
    ...
  )
}


#' Create UI for async function block
#'
#' Similar to function_block_ui but with Run/Cancel buttons instead of auto-execution
#'
#' @param ns Namespace function
#' @param fn_text Initial function code
#' @return Shiny tagList
#' @noRd
async_function_block_ui <- function(ns, fn_text) {
  shiny::tagList(
    async_function_block_css(),

    shiny::div(
      class = "function-block-container",

      # Authoring surface: gear-toggled inline editor (pushes content below down).
      # Same as the other function blocks; the difference is execution is manual
      # via the Run button below (not auto on edit).
      gear_editor_ui(ns, fn_text, label = "Function code"),

      # Dynamic parameter inputs (the resting surface).
      shiny::div(
        class = "function-block-params",
        shiny::uiOutput(ns("dynamic_params"))
      ),

      # Run / Cancel buttons (async execution).
      shiny::div(
        class = "d-flex gap-2 mb-2",
        style = "margin-bottom: 10px;",
        bslib::input_task_button(
          ns("run"),
          "Run",
          class = "btn-primary btn-sm"
        ),
        shiny::actionButton(
          ns("cancel"),
          "Cancel",
          class = "btn-outline-danger btn-sm",
          disabled = TRUE
        )
      )
    )
  )
}


#' Create CSS for async function block (the async status badges only; layout
#' comes from the shared `.function-block-*` / gear styles in code-block.css).
#'
#' @return HTML style tag
#' @noRd
async_function_block_css <- function() {
  shiny::tags$style(shiny::HTML(
    "
    .async-status-initial {
      color: #6c757d; font-size: 0.875rem; padding: 10px;
      background: #f8f9fa; border-radius: 4px;
    }
    .async-status-running {
      color: #0d6efd; font-size: 0.875rem; padding: 10px;
      background: #e7f1ff; border-radius: 4px;
    }
    .async-status-error {
      color: #dc3545; font-size: 0.875rem; padding: 10px;
      background: #f8d7da; border-radius: 4px;
    }
    "
  ))
}


#' Render async result based on type
#'
#' @param result The result to render
#' @param session Shiny session
#' @return Shiny UI element
#' @noRd
render_async_result <- function(result, session) {
  if (is_html_renderable(result)) {
    # Ask the object for its HTML (gt, htmlwidgets, composer composed_table, ...).
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
    output_id <- "dt_output"
    session$output[[output_id]] <- DT::renderDataTable({
      DT::datatable(
        result,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    DT::dataTableOutput(session$ns(output_id))
  } else {
    # Fallback: print method as preformatted text
    shiny::pre(
      style = "background: #f8f9fa; padding: 10px; border-radius: 4px; overflow-x: auto;",
      paste(utils::capture.output(print(result)), collapse = "\n")
    )
  }
}


#' @export
block_eval.async_function_block <- function(x, expr, env, ...) {
  # expr is a bquote expression - evaluate it to get the result with status attributes
  eval(expr, env)
}

#' @export
block_ui.async_function_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @export
block_output.async_function_block <- function(x, result, session) {
  shiny::renderUI({
    # Check for async status attribute
    status <- attr(result, ".async_status")
    error_msg <- attr(result, ".async_error")

    if (is.null(status)) {
      # No status attribute - render result directly (shouldn't happen)
      return(render_async_result(result, session))
    }

    if (status == "initial") {
      shiny::div(
        class = "async-status-initial",
        shiny::icon("info-circle"),
        " Click 'Run' to execute"
      )
    } else if (status == "running") {
      shiny::div(
        class = "async-status-running",
        shiny::tags$span(
          class = "spinner-border spinner-border-sm me-2",
          role = "status"
        ),
        " Computation in progress..."
      )
    } else if (status == "error") {
      shiny::div(
        class = "async-status-error",
        shiny::icon("exclamation-triangle"),
        " Error: ", if (is.null(error_msg)) "Unknown error" else error_msg
      )
    } else if (status == "success") {
      # Remove the async attributes before rendering
      attr(result, ".async_status") <- NULL
      attr(result, ".async_error") <- NULL

      # Check if it's the empty list placeholder (for pending states that completed)
      if (is.list(result) && length(result) == 0) {
        shiny::div(
          class = "async-status-initial",
          shiny::icon("info-circle"),
          " No result returned"
        )
      } else {
        render_async_result(result, session)
      }
    } else {
      shiny::div(
        class = "async-status-initial",
        " Unknown status: ", status
      )
    }
  })
}
