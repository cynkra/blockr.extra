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
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.extra)
#'   options(blockr.tabular_display = blockr.ui::html_table_display)
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

  # The data SLOT -- the literal `.(data)` placeholder -- never a free `data`
  # symbol. The block declares `expr_type = "bquoted"`, so blockr substitutes
  # only `.()` terms and does NOT wrap the expression in `with(args, ...)`.
  # A bare `data` therefore works in the app (the runtime env binds it) and
  # silently breaks every EXPORT: blockr.outline emits the symbol verbatim,
  # it resolves up the search path to `utils::data` (a function), and each
  # downstream block dies with "no applicable method for 'filter' applied to
  # an object of class 'function'".
  #
  # Built by hand rather than through blockr.core::bbquote(): the expression
  # below DEFINES A FUNCTION, and bbquote()'s splice pass deletes a function
  # definition's NULL srcref slot -- present whenever the code was parsed
  # without srcrefs, i.e. from an INSTALLED package. That crashes with
  # "'names' attribute [4] must be the same length as the vector [3]" in
  # production while passing every load_all() dev session.
  dat <- call(".", as.name("data"))

  if (!nzchar(trimws(string))) {
    # dplyr::filter() with no conditions returns the data unchanged;
    # keeps the expression a language object (not a bare symbol).
    return(bquote(dplyr::filter(.(d)), list(d = dat)))
  }

  # grepl() rather than stringr::str_detect(): this expression is read by
  # humans in the generated report, so it should look like the filter someone
  # would have written. grepl() coerces its input through as.character()
  # itself -- factors, numerics and Dates all just work -- and takes
  # `ignore.case` directly, which removes both the as.character() wrapper and
  # the stringr::fixed(..., ignore_case = TRUE) dance the previous form
  # needed. It also drops stringr from the generated code's dependencies.
  bquote(
    dplyr::filter(
      .(d),
      dplyr::if_any(
        dplyr::everything(),
        function(x) grepl(.(p), x, ignore.case = TRUE)
      )
    ),
    list(d = dat, p = escape_regex(string))
  )
}

#' Escape regex metacharacters
#'
#' The search box means literal substring: typing `1.5` must not match `125`.
#' `grepl()` is used in regex mode (its `fixed = TRUE` silently ignores
#' `ignore.case`), so the search string is escaped instead. Ordinary terms
#' come through untouched, which is what keeps the generated code readable.
#'
#' @param x A character string.
#' @return `x` with regex metacharacters backslash-escaped.
#' @noRd
escape_regex <- function(x) {
  gsub("([][{}().^$*+?|\\\\])", "\\\\\\1", x)
}
