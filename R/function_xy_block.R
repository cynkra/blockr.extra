#' Function XY Transform Block
#'
#' A block that wraps a user-defined R function with two data inputs (x and y)
#' and automatically generates UI based on the function's argument defaults.
#' Similar to [new_function_block()] but accepts two data frames as input.
#'
#' @section Approach:
#' The function must have `x` as its first argument and `y` as its second
#' argument (the two input data frames). Additional arguments are introspected
#' and UI is generated based on defaults:
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
#' @param fn Character string containing R function code that transforms two
#'   data frames. The function must have `x` as first argument and `y` as second
#'   argument. Default values of other arguments determine the UI widgets.
#' @param ... Additional arguments passed to new_block
#'
#' @examples
#' # Define a simple merge function
#' blk <- new_function_xy_block(
#'   fn = "function(x, y, by = 'id', all = FALSE) {
#'     merge(x, y, by = by, all = all)
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
#'         merged = new_function_xy_block(
#'           fn = "function(x, y) {
#'             dplyr::left_join(x, y, by = 'name')
#'           }"
#'         )
#'       ),
#'       links = links(
#'         from = c("data1", "data2"),
#'         to = c("merged", "merged"),
#'         input = c("x", "y")
#'       )
#'     )
#'   )
#' }
#'
#' @importFrom blockr.core block_eval block_output block_ui
#' @export
new_function_xy_block <- function(
    fn = "function(x, y) { dplyr::bind_rows(x, y) }",
    ...) {

  fn <- as_fn_text(fn)
  parsed <- parse_function_code(fn)
  validate_function_args(
    parsed,
    c("x", "y"),
    "Function must have 'x' as first argument and 'y' as second argument"
  )

  blockr.core::new_block(
    server = function(id, x, y) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          # Setup common server infrastructure
          base <- setup_function_block_server(
            input = input,
            output = output,
            session = session,
            fn_text = fn,
            required_args = c("x", "y"),
            skip_args = c("x", "y"),
            error_message = "Function must have 'x' as first argument and 'y' as second argument"
          )

          # Build expression
          list(
            expr = shiny::reactive({
              base$r_version()
              current_fn <- base$r_fn()
              shiny::req(current_fn)
              params <- base$get_param_values()

              # Build: .fn(x, y, param1 = val1, param2 = val2, ...)
              fn_call <- as.call(c(list(quote(.fn), quote(x), quote(y)), params))

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
        hint_text = "Function must have 'x' and 'y' as first two arguments"
      )
    },
    class = "function_xy_block",
    allow_empty_state = TRUE,
    external_ctrl = "fn",
    ...
  )
}


#' @export
block_eval.function_xy_block <- function(x, expr, env, ...) {
  eval_with_plot_capture(expr, env)
}

#' @export
block_ui.function_xy_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @export
block_output.function_xy_block <- function(x, result, session) {
  render_dynamic_output(result, x, session)
}
