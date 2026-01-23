#' Function Variadic Transform Block
#'
#' A block that wraps a user-defined R function with variadic data inputs (...)
#' and automatically generates UI based on the function's argument defaults.
#' Similar to [new_function_block()] but accepts any number of data frames.
#'
#' @section Approach:
#' The function must have `...` as its first argument to receive the input
#' data frames. Additional arguments after `...` are introspected and UI is
#' generated based on defaults:
#' - Character vector with multiple elements -> selectInput (dropdown)
#' - Single numeric value -> numericInput
#' - Single logical value -> checkboxInput
#' - Single character value -> textInput
#'
#' @section Output Types:
#' Output type is auto-detected from the function's return value:
#' - If the function returns a `gt_tbl` object, it renders as a GT table
#' - Otherwise, it renders as an interactive DataTable
#'
#' @param fn Character string containing R function code that transforms multiple
#'   data frames. The function must have `...` as first argument. Default values
#'   of other arguments determine the UI widgets. Legacy: passing a function
#'   object is deprecated but still supported.
#' @param ... Additional arguments passed to new_block
#'
#' @examples
#' # Define a simple bind function
#' blk <- new_function_var_block(
#'   fn = "function(..., .id = NULL) {
#'     dplyr::bind_rows(..., .id = .id)
#'   }"
#' )
#'
#' if (interactive()) {
#'   library(blockr.core)
#'
#'   serve(
#'     new_board(
#'       blocks = list(
#'         data1 = new_dataset_block(dataset = "band_members", package = "dplyr"),
#'         data2 = new_dataset_block(dataset = "band_instruments", package = "dplyr"),
#'         data3 = new_dataset_block(dataset = "band_instruments2", package = "dplyr"),
#'         merged = new_function_var_block(
#'           fn = "function(..., by = 'id') {
#'             dfs <- list(...)
#'             Reduce(function(x, y) merge(x, y, by = by, all = TRUE), dfs)
#'           }"
#'         )
#'       ),
#'       links = links(
#'         from = c("data1", "data2", "data3"),
#'         to = c("merged", "merged", "merged"),
#'         input = c("1", "2", "3")
#'       )
#'     )
#'   )
#' }
#'
#' @importFrom blockr.core block_output block_ui
#' @export
new_function_var_block <- function(
    fn = "function(...) { dplyr::bind_rows(...) }",
    ...) {

  # Legacy fallback: if fn is a function, convert to text with warning
  if (is.function(fn)) {
    warning(
      "Passing a function object to new_function_var_block() is deprecated. ",
      "Pass a character string instead.",
      call. = FALSE
    )
    fn <- paste(deparse(fn), collapse = "\n")
  }

  # fn should now be character
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

  # Validate first argument is '...'
  args <- names(formals(fn))
  if (length(args) == 0 || args[1] != "...") {
    stop("Function must have '...' as its first argument", call. = FALSE)
  }

# Helper function to extract argument names for variadic blocks
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

blockr.core::new_block(
  server = function(id, ...args) {
    shiny::moduleServer(
      id,
      function(input, output, session) {
        # Reactive to store the current function
        r_fn <- shiny::reactiveVal(fn)
        r_fn_text <- shiny::reactiveVal(fn_text)
        r_error <- shiny::reactiveVal(NULL)

        # Get argument names for variadic inputs
        arg_names <- shiny::reactive(
          stats::setNames(names(...args), dot_args_names(...args))
        )

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
              # Check that first arg is '...'
              args <- names(formals(evaluated))
              if (length(args) == 0 || args[1] != "...") {
                stop("Function must have '...' as its first argument")
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
          # Skip '...' argument
          args <- args[names(args) != "..."]

          if (length(args) == 0) {
            return(shiny::div(
              class = "text-muted",
              "No parameters to configure"
            ))
          }

          ui_elements <- lapply(names(args), function(arg_name) {
            default <- args[[arg_name]]
            create_input_for_arg_var(
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
          args <- args[names(args) != "..."]

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
            current_arg_names <- arg_names()

            # Build expression using do.call pattern
            # This handles variadic inputs properly
            bquote({
              .fn <- .(fn)
              .args <- list(..(dat))
              .params <- .(par)
              do.call(.fn, c(.args, .params))
            }, list(
              fn = current_fn,
              dat = lapply(current_arg_names, as.name),
              par = params
            ), splice = TRUE)
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
        .function-var-block-container {
          width: 100%%;
          padding-bottom: 10px;
        }
        .function-var-block-params {
          display: grid;
          gap: 15px;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          margin-bottom: 10px;
        }
        .function-var-block-params .shiny-input-container {
          width: 100%% !important;
        }
        .function-var-block-params .form-group {
          width: 100%%;
          margin-bottom: 0;
        }
        .function-var-block-params .form-control {
          width: 100%%;
        }
        .function-var-block-error {
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
        .block-advanced-toggle-var {
          cursor: pointer;
          user-select: none;
          padding: 8px 0;
          margin-bottom: 0;
          display: flex;
          align-items: center;
          gap: 6px;
          font-size: 0.8125rem;
        }
        .block-chevron-var {
          transition: transform 0.2s;
          display: inline-block;
          font-size: 14px;
          font-weight: bold;
        }
        .block-chevron-var.rotated {
          transform: rotate(90deg);
        }
        .function-var-editor-wrapper {
          border: 1px solid #dee2e6;
          border-radius: 4px;
          margin-top: 10px;
        }
        .function-var-editor-wrapper .shiny-ace {
          border: none;
        }
        ",
        advanced_id,
        advanced_id
      ))),

      shiny::div(
        class = "function-var-block-container",

        # Dynamic parameter inputs
        shiny::div(
          class = "function-var-block-params",
          shiny::uiOutput(ns("dynamic_params"))
        ),

        # Error display
        shiny::uiOutput(ns("error_display")),

        # Advanced toggle
        shiny::div(
          class = "block-advanced-toggle-var text-muted",
          id = ns("advanced-toggle"),
          onclick = sprintf(
            "
            const section = document.getElementById('%s');
            const chevron = document.querySelector('#%s .block-chevron-var');
            section.classList.toggle('expanded');
            chevron.classList.toggle('rotated');
            ",
            advanced_id,
            ns("advanced-toggle")
          ),
          shiny::tags$span(class = "block-chevron-var", "\u203A"),
          "Edit function"
        ),

        # Advanced options (function editor)
        shiny::div(
          id = advanced_id,
          shiny::div(
            style = "padding: 10px 0;",
            shiny::div(
              class = "function-var-editor-wrapper",
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
                "Function must have '...' as first argument"
              )
            )
          )
        )
      )
    )
  },
  dat_valid = function(...args) {
    stopifnot(length(...args) >= 1L)
  },
  class = "function_var_block",
  allow_empty_state = TRUE,
  ...
)
}


# Auto-detect output type from result:
# - gt_tbl objects render as GT
# - Everything else renders as DataTable

#' @export
block_ui.function_var_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @export
block_output.function_var_block <- function(x, result, session) {
  shiny::renderUI({
    if (inherits(result, "gt_tbl")) {
      # Result is already a gt object
      shiny::HTML(gt::as_raw_html(result))
    } else {
      # Default to DataTable for data.frames with board options
      dt_datatable(result, x, session)
    }
  })
}


#' Create input widget for a function argument (var block version)
#'
#' @param arg_name Name of the argument
#' @param default Default value from formals()
#' @param ns Namespace function
#' @return Shiny UI element
#' @noRd
create_input_for_arg_var <- function(arg_name, default, ns) {
input_id <- ns(paste0("param_", arg_name))
label <- gsub("_", " ", arg_name)
label <- gsub("^\\.", "", label)  # Remove leading dot from .id etc
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
  } else if (is.null(default_val)) {
    # NULL default -> textInput (common for .id parameters)
    shiny::textInput(
      inputId = input_id,
      label = label,
      value = "",
      placeholder = "NULL (leave empty)"
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
