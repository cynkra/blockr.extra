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
          # Setup common server infrastructure (params, editor, validation)
          base <- setup_function_block_server(
            input = input,
            output = output,
            session = session,
            fn = fn,
            fn_text = fn_text,
            required_args = "data",
            skip_args = "data",
            error_message = "Function must have 'data' as its first argument"
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
            state = list(
              fn = shiny::reactive({
                base$r_fn_text()
              })
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
  advanced_id <- ns("advanced-options")
  class_prefix <- "async-function-block"

  shiny::tagList(
    shinyjs::useShinyjs(),
    async_function_block_css(advanced_id, class_prefix),

    shiny::div(
      class = paste0(class_prefix, "-container"),

      # Dynamic parameter inputs
      shiny::div(
        class = paste0(class_prefix, "-params"),
        shiny::uiOutput(ns("dynamic_params"))
      ),

      # Run/Cancel buttons
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
      ),

      # Error display
      shiny::uiOutput(ns("error_display")),

      # Advanced toggle
      shiny::div(
        class = "block-advanced-toggle text-muted",
        id = ns("advanced-toggle"),
        onclick = sprintf(
          "
          const section = document.getElementById('%s');
          const chevron = document.querySelector('#%s .block-chevron');
          section.classList.toggle('expanded');
          chevron.classList.toggle('rotated');
          ",
          advanced_id,
          ns("advanced-toggle")
        ),
        shiny::tags$span(class = "block-chevron", "\u203A"),
        "Edit function"
      ),

      # Advanced options (function editor)
      shiny::div(
        id = advanced_id,
        shiny::div(
          style = "padding: 10px 0;",
          shiny::div(
            class = "function-editor-wrapper",
            shinyAce::aceEditor(
              outputId = ns("fn_code"),
              value = fn_text,
              mode = "r",
              theme = "tomorrow",
              height = "200px",
              fontSize = 13,
              showLineNumbers = TRUE,
              tabSize = 2,
              showPrintMargin = FALSE,
              highlightActiveLine = TRUE
            )
          ),
          shiny::div(
            style = "margin-top: 10px;",
            shiny::actionButton(
              ns("submit_fn"),
              "Apply Function",
              class = "btn-primary btn-sm"
            ),
            shiny::span(
              class = "text-muted",
              style = "margin-left: 10px; font-size: 0.8rem;",
              "Function must have 'data' as first argument"
            )
          )
        )
      )
    )
  )
}


#' Create CSS for async function block
#'
#' @param advanced_id Namespaced ID for advanced options div
#' @param class_prefix Prefix for CSS classes
#' @return HTML style tag
#' @noRd
async_function_block_css <- function(advanced_id, class_prefix = "async-function-block") {
  shiny::tags$style(shiny::HTML(sprintf(
    "
    .%s-container {
      width: 100%%;
      padding-bottom: 10px;
    }
    .%s-params {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      margin-bottom: 10px;
    }
    .%s-params .shiny-input-container {
      width: 100%% !important;
    }
    .%s-params .form-group {
      width: 100%%;
      margin-bottom: 0;
    }
    .%s-params .form-control {
      width: 100%%;
    }
    .async-status-initial {
      color: #6c757d;
      font-size: 0.875rem;
      padding: 10px;
      background: #f8f9fa;
      border-radius: 4px;
    }
    .async-status-running {
      color: #0d6efd;
      font-size: 0.875rem;
      padding: 10px;
      background: #e7f1ff;
      border-radius: 4px;
    }
    .async-status-error {
      color: #dc3545;
      font-size: 0.875rem;
      padding: 10px;
      background: #f8d7da;
      border-radius: 4px;
    }
    #%s {
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.3s ease-out;
    }
    #%s.expanded {
      max-height: 800px;
      overflow: visible;
      transition: max-height 0.5s ease-in;
    }
    .block-advanced-toggle {
      cursor: pointer;
      user-select: none;
      padding: 8px 0;
      margin-bottom: 0;
      display: flex;
      align-items: center;
      gap: 6px;
      font-size: 0.8125rem;
    }
    .block-chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .block-chevron.rotated {
      transform: rotate(90deg);
    }
    .function-editor-wrapper {
      border: 1px solid #dee2e6;
      border-radius: 4px;
      margin-top: 10px;
    }
    .function-editor-wrapper .shiny-ace {
      border: none;
    }
    ",
    class_prefix, class_prefix, class_prefix, class_prefix, class_prefix,
    advanced_id, advanced_id
  )))
}


#' Render async result based on type
#'
#' @param result The result to render
#' @param session Shiny session
#' @return Shiny UI element
#' @noRd
render_async_result <- function(result, session) {
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
