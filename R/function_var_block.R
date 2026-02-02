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

  fn_text <- fn
  fn <- parse_function_code(fn_text)
  validate_function_args(fn, "...", "Function must have '...' as its first argument")

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
          # Setup common server infrastructure
          base <- setup_function_block_server(
            input = input,
            output = output,
            session = session,
            fn = fn,
            fn_text = fn_text,
            required_args = "...",
            skip_args = "...",
            error_message = "Function must have '...' as its first argument",
            strip_leading_dot = TRUE
          )

          # Get argument names for variadic inputs
          arg_names <- shiny::reactive(
            stats::setNames(names(...args), dot_args_names(...args))
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
      function_block_ui(
        ns = ns,
        fn_text = fn_text,
        hint_text = "Function must have '...' as first argument",
        class_prefix = "function-block"
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
