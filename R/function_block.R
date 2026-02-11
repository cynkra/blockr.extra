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

  fn <- as_fn_text(fn)
  parsed <- parse_function_code(fn)
  validate_function_args(parsed, "data", "Function must have 'data' as its first argument")

  blockr.core::new_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Setup common server infrastructure
          base <- setup_function_block_server(
            input = input,
            output = output,
            session = session,
            fn_text = fn,
            required_args = "data",
            skip_args = "data",
            error_message = "Function must have 'data' as its first argument"
          )

          # Build expression
          list(
            expr = shiny::reactive({
              base$r_version()
              current_fn <- base$r_fn()
              shiny::req(current_fn)
              params <- base$get_param_values()

              # Build: .fn(data, param1 = val1, param2 = val2, ...)
              fn_call <- as.call(c(list(quote(.fn), quote(data)), params))

              bquote({
                .fn <- .(current_fn)
                .(fn_call)
              })
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
        hint_text = "Function must have 'data' as first argument"
      )
    },
    class = "function_block",
    allow_empty_state = TRUE,
    external_ctrl = "fn",
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
