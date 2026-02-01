#' Function Transform Block
#'
#' A block that wraps a user-defined R function and automatically generates
#' UI based on the function's argument defaults.
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
#' @param fn Character string containing R function code that transforms a data
#'   frame. The function must have `data` as first argument. Default values of
#'   other arguments determine the UI widgets.
#' @param ... Additional arguments passed to new_block
#'
#' @examples
#' # Define a simple filter function (renders as DataTable)
#' blk <- new_function_block(
#'   fn = "function(data, species = c('setosa', 'versicolor', 'virginica')) {
#'     dplyr::filter(data, Species == species)
#'   }"
#' )
#'
#' # Function returning GT object (auto-detected, renders as GT)
#' blk_gt <- new_function_block(
#'   fn = "function(data, title = 'My Table') {
#'     gt::gt(data) |> gt::tab_header(title = title)
#'   }"
#' )
#'
#' @importFrom blockr.core block_eval block_output block_ui
#' @export
new_function_block <- function(
    fn = "function(data, n = 6L) { utils::head(data, n) }",
    ...) {

  fn_text <- fn

  # Parse text to function
  fn <- tryCatch(
    eval(parse(text = fn_text)),
    error = function(e) {
      stop("Failed to parse function: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!is.function(fn)) {
    stop("fn must evaluate to a function", call. = FALSE)
  }

  # Validate first argument is 'data'
  args <- names(formals(fn))
  if (length(args) == 0 || args[1] != "data") {
    stop("Function must have 'data' as its first argument", call. = FALSE)
  }

blockr.core::new_block(
  server = function(id, data) {
    shiny::moduleServer(
      id,
      function(input, output, session) {
        # Reactive to store the current function
        r_fn <- shiny::reactiveVal(fn)
        r_fn_text <- shiny::reactiveVal(fn_text)
        r_error <- shiny::reactiveVal(NULL)

        # Parse function when code changes
        shiny::observeEvent(input$submit_fn, {
          code <- input$fn_code
          if (is.null(code) || trimws(code) == "") {
            r_error("Function code is empty")
            return()
          }

          result <- tryCatch(
            {
              parsed <- parse(text = code)
              evaluated <- eval(parsed)
              if (!is.function(evaluated)) {
                stop("Code must evaluate to a function")
              }
              # Check that first arg is 'data'
              args <- names(formals(evaluated))
              if (length(args) == 0 || args[1] != "data") {
                stop("Function must have 'data' as its first argument")
              }
              list(success = TRUE, fn = evaluated, text = code)
            },
            error = function(e) {
              list(success = FALSE, error = conditionMessage(e))
            }
          )

          if (result$success) {
            r_fn(result$fn)
            r_fn_text(result$text)
            r_error(NULL)
          } else {
            r_error(result$error)
          }
        })

        # Generate dynamic UI based on function arguments
        output$dynamic_params <- shiny::renderUI({
          current_fn <- r_fn()
          shiny::req(current_fn)

          args <- formals(current_fn)
          # Skip 'data' argument
          args <- args[names(args) != "data"]

          if (length(args) == 0) {
            return(shiny::div(
              class = "text-muted",
              "No parameters to configure"
            ))
          }

          ui_elements <- lapply(names(args), function(arg_name) {
            default <- args[[arg_name]]
            create_input_for_arg(
              arg_name = arg_name,
              default = default,
              ns = session$ns
            )
          })

          shiny::tagList(ui_elements)
        })

        # Collect current parameter values
        get_param_values <- shiny::reactive({
          current_fn <- r_fn()
          shiny::req(current_fn)

          args <- formals(current_fn)
          args <- args[names(args) != "data"]

          if (length(args) == 0) {
            return(list())
          }

          values <- lapply(names(args), function(arg_name) {
            input_id <- paste0("param_", arg_name)
            val <- input[[input_id]]
            # Handle empty strings from textInput
            if (is.character(val) && length(val) == 1 && val == "") {
              val <- NULL
            }
            val
          })
          names(values) <- names(args)

          # Remove NULL values (not yet initialized)
          values <- values[!vapply(values, is.null, logical(1))]

          # Wait until we have at least some parameters or none are expected
          if (length(values) == 0 && length(args) > 0) {
            # Parameters not yet initialized, use defaults
            return(list())
          }

          values
        })

        # Build expression
        list(
          expr = shiny::reactive({
            current_fn <- r_fn()
            shiny::req(current_fn)
            params <- get_param_values()

            # Build the function call syntactically
            # Creates: .fn(data, param1 = val1, param2 = val2, ...)
            fn_call <- as.call(c(list(quote(.fn), quote(data)), params))

            bquote({
              .fn <- .(current_fn)
              .(fn_call)
            })
          }),
          state = list(
            fn = shiny::reactive({
              r_fn_text()
            })
          )
        )
      }
    )
  },
  ui = function(id) {
    ns <- shiny::NS(id)
    advanced_id <- ns("advanced-options")

    shiny::tagList(
      shinyjs::useShinyjs(),

      # CSS for layout
      shiny::tags$style(shiny::HTML(sprintf(
        "
        .function-block-container {
          width: 100%%;
          padding-bottom: 10px;
        }
        .function-block-params {
          display: grid;
          gap: 15px;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          margin-bottom: 10px;
        }
        .function-block-params .shiny-input-container {
          width: 100%% !important;
        }
        .function-block-params .form-group {
          width: 100%%;
          margin-bottom: 0;
        }
        .function-block-params .form-control {
          width: 100%%;
        }
        .function-block-error {
          color: #dc3545;
          font-size: 0.875rem;
          margin-top: 5px;
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
        advanced_id,
        advanced_id
      ))),

      shiny::div(
        class = "function-block-container",

        # Dynamic parameter inputs
        shiny::div(
          class = "function-block-params",
          shiny::uiOutput(ns("dynamic_params"))
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
  },
  class = "function_block",
  allow_empty_state = TRUE,
  ...
)
}


#' @export
block_eval.function_block <- function(x, expr, env, ...) {
  eval_with_plot_capture(expr, env)
}

#' @export
block_ui.function_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @export
block_output.function_block <- function(x, result, session) {
  render_dynamic_output(result, x, session)
}


#' Create input widget for a function argument
#'
#' @param arg_name Name of the argument
#' @param default Default value from formals()
#' @param ns Namespace function
#' @return Shiny UI element
#' @noRd
create_input_for_arg <- function(arg_name, default, ns) {
input_id <- ns(paste0("param_", arg_name))
label <- gsub("_", " ", arg_name)
label <- paste0(toupper(substr(label, 1, 1)), substring(label, 2))

# Evaluate the default if it's a call/expression
default_val <- tryCatch(
  eval(default),
  error = function(e) default
)

shiny::div(
  class = "block-input-wrapper",
  if (is.character(default_val) && length(default_val) > 1) {
    # Multiple character values -> selectInput
    shiny::selectInput(
      inputId = input_id,
      label = label,
      choices = default_val,
      selected = default_val[1]
    )
  } else if (is.numeric(default_val) && length(default_val) == 1) {
    # Single numeric -> numericInput
    shiny::numericInput(
      inputId = input_id,
      label = label,
      value = default_val
    )
  } else if (is.logical(default_val) && length(default_val) == 1) {
    # Single logical -> checkboxInput
    shiny::checkboxInput(
      inputId = input_id,
      label = label,
      value = default_val
    )
  } else if (is.character(default_val) && length(default_val) == 1) {
    # Single character -> textInput
    shiny::textInput(
      inputId = input_id,
      label = label,
      value = default_val
    )
  } else if (is.numeric(default_val) && length(default_val) > 1) {
    # Multiple numeric values -> selectInput (treat as choices)
    shiny::selectInput(
      inputId = input_id,
      label = label,
      choices = default_val,
      selected = default_val[1]
    )
  } else {
    # Fallback: show as text
    shiny::textInput(
      inputId = input_id,
      label = paste(label, "(unsupported type)"),
      value = if (is.null(default_val)) "" else as.character(default_val)[1]
    )
  }
)
}
