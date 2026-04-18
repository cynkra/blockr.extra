#' Search block
#'
#' Filter rows of a data frame by case-insensitive substring match across all
#' columns. Equivalent to DataTables' global search: a single text input,
#' every cell coerced to character, a row is kept if any of its cells contains
#' the search string.
#'
#' @param string Initial search string. Empty string is a no-op.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A transform block of class `search_block`.
#'
#' @importFrom stringr str_detect fixed
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.extra)
#'   options(blockr.html_table_preview = TRUE)
#'   serve(new_search_block(), data = list(data = iris))
#' }
#'
#' @export
new_search_block <- function(string = "", ...) {
  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_string <- shiny::reactiveVal(string)

          shiny::observeEvent(input$string, r_string(input$string))

          r_debounced <- shiny::debounce(r_string, 300)

          list(
            expr = shiny::reactive(make_search_expr(r_debounced())),
            state = list(string = r_string)
          )
        }
      )
    },
    function(id) {
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::textInput(
            shiny::NS(id, "string"),
            label = NULL,
            value = string,
            placeholder = "Type to filter rows\u2026",
            width = "100%"
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Search block requires a data frame as input.")
      }
    },
    expr_type = "bquoted",
    allow_empty_state = TRUE,
    class = "search_block",
    ...
  )
}

#' Build the search block expression
#'
#' Returns a quoted expression that, when evaluated in an environment where
#' `data` is bound to a data frame, filters rows whose cells (after
#' coercion to character) contain `string` as a case-insensitive substring.
#' Empty or whitespace-only strings short-circuit to `data` unchanged.
#'
#' @param string Search string.
#'
#' @return A language object.
#'
#' @noRd
make_search_expr <- function(string) {
  if (!nzchar(trimws(string))) {
    # dplyr::filter() with no conditions returns the data unchanged;
    # keeps the expression a language object (not a bare symbol).
    return(quote(dplyr::filter(data)))
  }
  bquote(
    dplyr::filter(
      data,
      dplyr::if_any(
        dplyr::everything(),
        function(.col) stringr::str_detect(
          as.character(.col),
          stringr::fixed(.(s), ignore_case = TRUE)
        )
      )
    ),
    list(s = string)
  )
}
