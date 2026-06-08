#' Shared utilities for function blocks
#'
#' Internal functions used by function_block, function_var_block, and
#' function_xy_block to reduce code duplication.
#'
#' @name function-block-base
#' @keywords internal
NULL


#' Coerce to reactiveVal
#'
#' If `x` is already a reactiveVal, return it; otherwise create one with
#' `default` as initial value.
#'
#' @param x Value to coerce
#' @param default Initial value if a new reactiveVal is created
#' @return A reactiveVal
#' @noRd
as_rv <- function(x, default = x) {
  if (inherits(x, "reactiveVal")) x else shiny::reactiveVal(default)
}


#' Ensure fn is a character string
#'
#' If `fn` is already a function, deparse it back to a string.
#' If it's a string, return as-is.
#'
#' @param fn A function object or character string of R function code
#' @return Character string
#' @noRd
as_fn_text <- function(fn) {
  if (is.function(fn)) {
    paste(deparse(fn), collapse = "\n")
  } else {
    stopifnot(is.character(fn), length(fn) == 1L)
    fn
  }
}


#' Parse function code from text
#'
#' @param fn_text Character string containing R function code
#' @return Parsed function object
#' @noRd
parse_function_code <- function(fn_text) {
  fn <- tryCatch(
    eval(parse(text = fn_text)),
    error = function(e) {
      stop("Failed to parse function: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!is.function(fn)) {
    stop("fn must evaluate to a function", call. = FALSE)
  }

  fn
}


#' Validate function arguments
#'
#' @param fn Function to validate
#' @param required_args Character vector of required argument names (in order)
#' @param error_message Error message if validation fails
#' @noRd
validate_function_args <- function(fn, required_args, error_message) {
  args <- names(formals(fn))
  n_required <- length(required_args)

  if (length(args) < n_required) {
    stop(error_message, call. = FALSE)
  }

  for (i in seq_along(required_args)) {
    if (args[i] != required_args[i]) {
      stop(error_message, call. = FALSE)
    }
  }

  invisible(TRUE)
}


#' Create function editor UI
#'
#' The standard gear button (top-right) toggles an inline editor section that
#' pushes the params/output below it down. There is no block-private error
#' panel: a *syntax* error disables Run in the editor footer (you can't run
#' un-parseable code), and a *runtime* error surfaces through the normal blockr
#' evaluation system like any other block.
#'
#' @param ns Namespace function
#' @param fn_text Initial function code
#' @param hint_text Unused (kept for back-compatible call sites).
#' @param class_prefix Prefix for CSS classes
#' @return Shiny tagList
#' @noRd
function_block_ui <- function(ns, fn_text, hint_text, class_prefix = "function-block") {
  shiny::tagList(
    shiny::div(
      class = paste0(class_prefix, "-container"),

      # Authoring surface: gear-toggled inline editor (pushes content below down).
      gear_editor_ui(ns, fn_text, label = "Function code"),

      # Dynamic parameter inputs (the resting surface).
      shiny::div(
        class = paste0(class_prefix, "-params"),
        shiny::uiOutput(ns("dynamic_params"))
      )
    )
  )
}


#' Create input widget for a function argument
#'
#' @param arg_name Name of the argument
#' @param default Default value from formals()
#' @param ns Namespace function
#' @param strip_leading_dot Whether to strip leading dots from labels (for .id etc)
#' @return Shiny UI element
#' @noRd
create_input_for_arg <- function(arg_name, default, ns, strip_leading_dot = FALSE) {
  input_id <- ns(paste0("param_", arg_name))
  label <- gsub("_", " ", arg_name)
  if (strip_leading_dot) {
    label <- gsub("^\\.", "", label)
  }
  label <- paste0(toupper(substr(label, 1, 1)), substring(label, 2))

  # Handle missing defaults (arguments without default values)
  if (identical(default, quote(expr = ))) {
    return(shiny::div(
      class = "block-input-wrapper",
      shiny::textInput(
        inputId = input_id,
        label = label,
        value = "",
        placeholder = "No default"
      )
    ))
  }

  # Evaluate the default if it's a call/expression
  default_val <- tryCatch(
    eval(default),
    error = function(e) default
  )

  shiny::div(
    class = "block-input-wrapper",
    if (is.list(default_val) && !is.data.frame(default_val)) {
      # list() -> multi-select (names become labels, values are actual values)
      choices <- unlist(default_val)
      shiny::selectInput(
        inputId = input_id,
        label = label,
        choices = choices,
        selected = unname(choices),
        multiple = TRUE
      )
    } else if (is.character(default_val) && length(default_val) > 1) {
      # c() with multiple values -> single selectInput (names become labels)
      shiny::selectInput(
        inputId = input_id,
        label = label,
        choices = default_val,
        selected = unname(default_val[1])
      )
    } else if (is.numeric(default_val) && length(default_val) > 1) {
      # Numeric vector -> single selectInput (names become labels)
      shiny::selectInput(
        inputId = input_id,
        label = label,
        choices = default_val,
        selected = unname(default_val[1])
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


#' Setup common server reactives for function blocks
#'
#' Creates the shared reactive infrastructure used by all function block types.
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param fn_text Initial function code text (string or reactiveVal)
#' @param required_args Character vector of required argument names
#' @param skip_args Character vector of argument names to skip in UI
#' @param error_message Error message for validation failure
#' @param strip_leading_dot Whether to strip leading dots from parameter labels
#' @return List with r_fn, r_fn_text, r_error, r_version reactiveVals and get_param_values reactive
#' @noRd
setup_function_block_server <- function(
    input,
    output,
    session,
    fn_text,
    required_args,
    skip_args,
    error_message,
    strip_leading_dot = FALSE
) {
  # Reactive values to store current function state
  r_fn_text <- as_rv(fn_text, fn_text)
  r_fn <- shiny::reactiveVal(parse_function_code(shiny::isolate(r_fn_text())))
  r_error <- shiny::reactiveVal(NULL)
  r_version <- shiny::reactiveVal(0L)

  # Parse/validate on every change of the code text. The editor reverse-sync
  # (pushing AI/external writes into the editor + the inline diff) is handled by
  # setup_code_editor_server (blockr-code-set), not here.
  shiny::observeEvent(r_fn_text(), {
    result <- tryCatch({
      parsed <- eval(parse(text = r_fn_text()))
      if (!is.function(parsed)) stop("Code must evaluate to a function")
      validate_function_args(parsed, required_args, error_message)
      list(success = TRUE, fn = parsed)
    }, error = function(e) list(success = FALSE, error = conditionMessage(e)))
    if (result$success) {
      r_fn(result$fn)
      r_error(NULL)
      r_version(r_version() + 1L)
    } else {
      r_error(result$error)
    }
  }, ignoreInit = TRUE)

  # Parse and validate function when user clicks Apply
  shiny::observeEvent(input$submit_fn, {
    code <- input$fn_code
    if (is.null(code) || trimws(code) == "") {
      r_error("Function code is empty")
      return()
    }

    result <- tryCatch({
      parsed <- eval(parse(text = code))
      if (!is.function(parsed)) stop("Code must evaluate to a function")
      validate_function_args(parsed, required_args, error_message)
      list(success = TRUE, fn = parsed, text = code)
    }, error = function(e) list(success = FALSE, error = conditionMessage(e)))

    if (result$success) {
      r_fn_text(result$text)
      r_fn(result$fn)
      r_error(NULL)
      r_version(r_version() + 1L)
    } else {
      r_error(result$error)
    }
  })

# Display errors to user
output$error_display <- shiny::renderUI({
  err <- r_error()
  if (!is.null(err)) {
    shiny::div(
      class = "function-block-error",
      shiny::icon("exclamation-triangle"),
      " ",
      err
    )
  }
})

# Generate dynamic UI based on function arguments
output$dynamic_params <- shiny::renderUI({
  current_fn <- r_fn()
  shiny::req(current_fn)

  args <- formals(current_fn)
  args <- args[!names(args) %in% skip_args]

  if (length(args) == 0) {
    # No params: render nothing (stay blank) rather than a placeholder note.
    return(NULL)
  }

  ui_elements <- lapply(names(args), function(arg_name) {
    default <- args[[arg_name]]
    create_input_for_arg(
      arg_name = arg_name,
      default = default,
      ns = session$ns,
      strip_leading_dot = strip_leading_dot
    )
  })

  shiny::tagList(ui_elements)
})

# Collect current parameter values
get_param_values <- shiny::reactive({
  current_fn <- r_fn()
  shiny::req(current_fn)

  args <- formals(current_fn)
  args <- args[!names(args) %in% skip_args]

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
    return(list())
  }

  values
})

list(
  r_fn = r_fn,
  r_fn_text = r_fn_text,
  r_error = r_error,
  r_version = r_version,
  get_param_values = get_param_values
)
}
