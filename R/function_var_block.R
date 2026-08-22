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
#' Output type is auto-detected from the function's return value (in priority order):
#' - `gt_tbl`: renders as GT HTML table
#' - `ggplot`: renders with plotOutput/renderPlot
#' - `recordedplot` (base R plot): renders with evaluate::replay() in renderPlot
#' - `data.frame`/`tibble`: renders as interactive DataTable
#' - Any other object: shows print() output as preformatted text
#'
#' @param fn Character string containing R function code that transforms multiple
#'   data frames. The function must have `...` as first argument. Default values
#'   of other arguments determine the UI widgets.
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
#' @importFrom blockr.core block_eval block_output block_ui
#' @export
new_function_var_block <- function(
    fn = "function(...) { dplyr::bind_rows(...) }",
    ...) {

  fn <- as_fn_text(fn)
  parsed <- parse_function_code(fn)
  validate_function_args(parsed, "...", "Function must have '...' as its first argument")

  # Variadic ...args helpers, mirroring blockr.core (not exported). Unnamed
  # slots (added by dragging an edge in the DAG UI) are stored positionally, so
  # names(...args) is "" / NULL for them; the old names-based helper collapsed
  # that to NULL, dropping the connected input (do.call with no data). Each slot
  # is bound in the eval env under a symbol: the link name, or .arg1/.arg2/...
  # for unnamed ones. dot_arg_refs() returns those symbols keyed by display
  # name; dot_arg_values() pairs them with realized values. Keep in sync with
  # blockr.core R/utils-misc.R.
  dot_sym <- function(i) {
    paste0(".arg", i)
  }

  arg_refs <- function(nms) {
    unnamed <- !nzchar(nms)
    replace(nms, unnamed, dot_sym(seq_len(sum(unnamed))))
  }

  dot_arg_refs <- function(x) {
    nms <- names(x)
    if (is.null(nms)) {
      nms <- character(length(x))
    }
    stats::setNames(arg_refs(nms), nms)
  }

  dot_arg_values <- function(x) {
    vals <- if (inherits(x, "reactivevalues")) {
      shiny::reactiveValuesToList(x)
    } else {
      as.list(x)
    }
    stats::setNames(vals, unname(dot_arg_refs(x)))
  }

  blockr.core::new_block(
    server = function(id, ...args) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Setup common server infrastructure
          base <- setup_function_block_server(
            input = input,
            output = output,
            session = session,
            fn_text = fn,
            required_args = "...",
            skip_args = "...",
            error_message = "Function must have '...' as its first argument",
            strip_leading_dot = TRUE
          )

          # The shared Blockr.Code editor (autocomplete over all inputs' cols).
          setup_code_editor_server(
            input, output, session, base,
            cols = shiny::reactive({
              vals <- dot_arg_values(...args)
              unique(unlist(lapply(vals, function(v) {
                tryCatch(
                  names(if (shiny::is.reactive(v)) v() else v),
                  error = function(e) NULL
                )
              })))
            }),
            required_args = "...",
            contract_message = "Function must have '...' as its first argument"
          )

          # Eval-env symbols for the connected inputs (.arg1, .arg2, ... for
          # unnamed DAG-UI slots, else the link name); reactive on the link set.
          arg_names <- shiny::reactive(
            dot_arg_refs(...args)
          )

          # Build expression
          list(
            expr = shiny::reactive({
              base$r_version()
              current_fn <- base$r_fn()
              shiny::req(current_fn)
              params <- base$get_param_values()
              current_arg_names <- arg_names()

              # Build expression using do.call pattern for variadic inputs
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
              fn = base$r_fn_text
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      function_block_ui(
        ns = ns,
        fn_text = fn,
        hint_text = "Function must have '...' as first argument",
        class_prefix = "function-block"
      )
    },
    dat_valid = function(...args) {
      # Zero inputs is allowed: with `...` the block can act as a pure source
      # (e.g. open a DB connection / read a file in the body and return a
      # table), so it does not require any upstream data. One or more inputs
      # is the merge / bind use.
      TRUE
    },
    class = "function_var_block",
    # `input = TRUE` keeps the `fn` state field from wedging when cleared;
    # `data = list(...args = 0)` tells core the block needs ZERO variadic
    # inputs, so it can act as a pure source (read a file / open a DB in the
    # body) instead of sitting in "waiting" on start. Without the `...args`
    # entry, core defaults min_args to 1L and gates the block on one input.
    allow_empty_state = list(input = TRUE, data = list(...args = 0L)),
    external_ctrl = "fn",
    ...
  )
}


#' @export
block_eval.function_var_block <- function(x, expr, env, ...) {
  eval_with_plot_capture(expr, env)
}

#' @export
block_ui.function_var_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @export
block_output.function_var_block <- function(x, result, session) {
  render_dynamic_output(result, x, session)
}
