#' Read filter conditions off a drilled annotated data frame
#'
#' Turns the drilled subset of an annotated (ARD-shaped) data frame into the
#' `columns` payload a value filter takes, so a sender can push it with
#' [ctrl_send()]. This is the claim logic behind [new_ctrl_filter_block()],
#' exported so a [new_function_block()] prototyping a bespoke sender can call
#' it instead of carrying its own copy.
#'
#' @section Two claim modes:
#' **Annotated (`columns = NULL`, the default) -- table and summary drills.**
#' The drilled data frame is read on its ARD identity columns: `.variable` is
#' the source column *name*, `.variable_level` the raw source *value* on that
#' row, and each `.group<k>` / `.group<k>_level` pair is an enclosing grouping
#' dimension. Dimensions come back outermost first (`.group1`, `.group2`, ...),
#' with the `.variable` leaf last.
#'
#' **Source columns (`columns` given) -- chart and tile drills.** A chart does
#' not carry the ARD identity: it coerces its input with `as_plain_df()`, which
#' *drops* the `.variable` columns, and its drilled output is a plain row subset
#' of the source data. There is nothing in that subset saying which column was
#' drilled, so the caller names the candidates -- `columns = "SEX"`, or several
#' if the chart's drill column varies -- and each is claimed from the subset.
#'
#' @section What counts as a claim:
#' Either way, a dimension becomes a filter condition only when the subset
#' resolves it to **exactly one** value: one value is a decision, many are not.
#'
#' That single rule is what makes an un-drill propagate. Clicking one bar (or
#' one level row) leaves `SEX` single-valued -- a claim. An *undrilled* chart or
#' table passes all its rows through, so `SEX` still holds both `F` and `M` --
#' no claim, and the caller's cue to [ctrl_clear()]. A brush across several bars
#' is likewise deliberately *not* a claim, and neither is a header row in a
#' nested table whose leaf stays multi-valued (its outer group is still claimed).
#'
#' @param data A data frame, typically the drilled output of a table, chart or
#'   tile block. Anything that resolves no dimension yields an empty list rather
#'   than an error.
#' @param table Character(1). Name of the table in the target's `dm` that the
#'   conditions apply to.
#' @param mode Character(1). Filter mode for each condition, e.g. `"multi"`.
#' @param columns Character vector of *source* column names to claim, for
#'   drilled output that carries no ARD identity (charts, tiles). `NULL` (the
#'   default) reads the ARD identity columns instead.
#'
#' @return A list of `list(name=, table=, mode=, values=)` entries, one per
#'   claimed dimension. Empty when nothing is claimed.
#'
#' @examples
#' # Annotated mode: a drilled summary table.
#' drilled <- data.frame(
#'   .variable = "SEX", .variable_level = "F", n = 143
#' )
#' drill_claim_columns(drilled, table = "adsl")
#'
#' # Source-column mode: a chart drilled to one bar.
#' drill_claim_columns(
#'   data.frame(USUBJID = c("01-001", "01-002"), SEX = c("F", "F")),
#'   table = "adsl",
#'   columns = "SEX"
#' )
#'
#' @export
drill_claim_columns <- function(data, table, mode = "multi", columns = NULL) {

  stopifnot(
    is.character(table), length(table) == 1L,
    is.character(mode), length(mode) == 1L,
    is.null(columns) || is.character(columns)
  )

  if (!is.data.frame(data)) {
    return(list())
  }

  nms <- names(data)

  cond <- function(name, values) {
    list(name = name, table = table, mode = mode, values = values)
  }

  # Source-column mode: no ARD identity to read (chart / tile drills), so the
  # caller names the columns a click may claim.
  if (length(columns)) {
    claims <- list()

    for (col in columns) {
      if (!col %in% nms) {
        next
      }
      value <- single_value(as.character(data[[col]]))
      if (is.null(value)) {
        next
      }
      claims[[length(claims) + 1L]] <- cond(col, value)
    }

    return(claims)
  }

  if (!all(c(".variable", ".variable_level") %in% nms)) {
    return(list())
  }

  var <- as.character(data$.variable)
  lvl <- as.character(data$.variable_level)
  keep <- !is.na(var) & !is.na(lvl) & nzchar(lvl)

  if (!any(keep)) {
    return(list())
  }

  cols <- list()

  # Enclosing group dimensions, outermost first.
  for (g in group_level_cols(nms)) {
    gname <- substr(g, 1L, nchar(g) - 6L)
    if (!gname %in% nms) {
      next
    }
    name <- single_value(as.character(data[[gname]])[keep])
    value <- single_value(as.character(data[[g]])[keep])
    if (is.null(name) || is.null(value)) {
      next
    }
    cols[[length(cols) + 1L]] <- cond(name, value)
  }

  # The `.variable` leaf.
  name <- single_value(var[keep])
  value <- single_value(lvl[keep])

  if (!is.null(name) && !is.null(value)) {
    cols[[length(cols) + 1L]] <- cond(name, value)
  }

  cols
}

# `.group<k>_level` columns, ordered by k.
group_level_cols <- function(nms) {
  gl <- nms[startsWith(nms, ".group") & endsWith(nms, "_level")]
  gl[order(as.integer(substr(gl, 7L, nchar(gl) - 6L)))]
}

# The single distinct value of `x`, or NULL if it does not resolve to one.
single_value <- function(x) {
  u <- unique(x[!is.na(x) & nzchar(x)])
  if (length(u) == 1L) u else NULL
}
