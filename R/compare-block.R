#' Compare Block
#'
#' Compares two data frames with the same shape by joining on key columns
#' and computing a diff metric on measurement columns. Output contains key
#' columns plus one diff column per measurement, named identically to the
#' original measurement columns.
#'
#' @param key_cols Character vector of columns to join on
#' @param measure_cols Character vector of numeric columns to compare
#' @param join_type Join type: `"inner"` (default) or `"full"`
#' @param metric Diff metric: `"diff"`, `"abs_diff"`, `"rel_diff"`,
#'   `"ratio"`, or `"pct_change"`
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `compare_block`.
#'
#' @export
new_compare_block <- function(
    key_cols = character(),
    measure_cols = character(),
    join_type = c("inner", "full"),
    metric = c("diff", "abs_diff", "rel_diff", "ratio", "pct_change"),
    ...) {

  join_type <- match.arg(join_type)
  metric <- match.arg(metric)

  blockr.core::new_transform_block(
    function(id, x, y) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_key_cols <- blockr.dplyr::column_picker_server(
            "key_picker",
            get_choices = shiny::reactive(intersect(colnames(x()), colnames(y()))),
            initial_value = shiny::reactiveVal(key_cols)
          )
          r_measure_cols <- blockr.dplyr::column_picker_server(
            "measure_picker",
            get_choices = shiny::reactive(intersect(colnames(x()), colnames(y()))),
            initial_value = shiny::reactiveVal(measure_cols)
          )
          r_join_type <- shiny::reactiveVal(join_type)
          r_metric <- shiny::reactiveVal(metric)

          # One-shot heuristic: fill defaults on first data load
          shiny::observe({
            h <- classify_columns(x(), y())
            if (length(r_key_cols()) == 0) r_key_cols(h$key_cols)
            if (length(r_measure_cols()) == 0) r_measure_cols(h$measure_cols)
          }) |> shiny::bindEvent(x(), y(), once = TRUE)

          shiny::observeEvent(input$join_type, r_join_type(input$join_type))
          shiny::observeEvent(input$metric, r_metric(input$metric))

          list(
            expr = shiny::reactive(
              blockr.core::bbquote(
                blockr.extra::compare_frames(
                  .(x), .(y),
                  key_cols = .(kc),
                  measure_cols = .(mc),
                  join_type = .(jt),
                  metric = .(mt)
                ),
                list(
                  kc = r_key_cols(),
                  mc = r_measure_cols(),
                  jt = r_join_type(),
                  mt = r_metric()
                )
              )
            ),
            state = list(
              key_cols = r_key_cols,
              measure_cols = r_measure_cols,
              join_type = r_join_type,
              metric = r_metric
            )
          )
        }
      )
    },
    function(id) {
      shiny::tagList(
        shiny::tags$style(shiny::HTML("
          .block-container { width: 100%; padding-bottom: 10px; }
          .block-form-grid {
            display: grid;
            gap: 15px;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          }
          .block-input-wrapper { width: 100%; }
          .block-input-wrapper .form-group { margin-bottom: 10px; }
        ")),
        shiny::div(
          class = "block-container",
          shiny::div(
            class = "block-form-grid",
            shiny::div(
              class = "block-input-wrapper",
              blockr.dplyr::column_picker_ui(
                shiny::NS(id, "key_picker"),
                label = "Key columns",
                choices = key_cols,
                selected = key_cols
              )
            ),
            shiny::div(
              class = "block-input-wrapper",
              blockr.dplyr::column_picker_ui(
                shiny::NS(id, "measure_picker"),
                label = "Measurement columns",
                choices = measure_cols,
                selected = measure_cols
              )
            ),
            shiny::div(
              class = "block-input-wrapper",
              shiny::selectInput(
                inputId = shiny::NS(id, "join_type"),
                label = "Join type",
                choices = c("Inner" = "inner", "Full" = "full"),
                selected = join_type,
                width = "100%"
              )
            ),
            shiny::div(
              class = "block-input-wrapper",
              shiny::selectInput(
                inputId = shiny::NS(id, "metric"),
                label = "Diff metric",
                choices = c(
                  "Difference" = "diff",
                  "Absolute difference" = "abs_diff",
                  "Relative difference (%)" = "rel_diff",
                  "Ratio" = "ratio",
                  "Percent change" = "pct_change"
                ),
                selected = metric,
                width = "100%"
              )
            )
          )
        )
      )
    },
    dat_valid = function(x, y) {
      stopifnot(is.data.frame(x), is.data.frame(y))
    },
    expr_type = "bquoted",
    allow_empty_state = c("key_cols", "measure_cols"),
    class = "compare_block",
    ...
  )
}

#' Compare two data frames
#'
#' Joins two data frames on key columns and computes a diff metric for each
#' measurement column. When a single metric is selected the output columns
#' keep their original names; division-by-zero produces `NA`.
#'
#' @param x,y Data frames to compare
#' @param key_cols Character vector of columns to join on
#' @param measure_cols Character vector of numeric columns to diff
#' @param join_type `"inner"` or `"full"`
#' @param metric One of `"diff"`, `"abs_diff"`, `"rel_diff"`, `"ratio"`,
#'   `"pct_change"`
#'
#' @return A data frame with key columns and one diff column per measurement.
#'
#' @export
compare_frames <- function(x, y, key_cols, measure_cols,
                           join_type = "inner", metric = "diff") {

  x <- x[, c(key_cols, measure_cols), drop = FALSE]
  y <- y[, c(key_cols, measure_cols), drop = FALSE]

  join_fn <- switch(
    join_type,
    inner = dplyr::inner_join,
    full = dplyr::full_join
  )

  joined <- join_fn(x, y, by = key_cols, suffix = c("__x", "__y"))

  result <- joined[, key_cols, drop = FALSE]

  for (col in measure_cols) {
    xc <- joined[[paste0(col, "__x")]]
    yc <- joined[[paste0(col, "__y")]]

    result[[col]] <- switch(
      metric,
      diff = xc - yc,
      abs_diff = abs(xc - yc),
      rel_diff = ifelse(yc == 0, NA_real_, (xc - yc) / yc * 100),
      ratio = ifelse(yc == 0, NA_real_, xc / yc),
      pct_change = {
        avg <- (xc + yc) / 2
        ifelse(avg == 0, NA_real_, (xc - yc) / avg * 100)
      }
    )
  }

  result
}

#' Heuristic column classification
#'
#' Classifies shared columns of two data frames into key columns (used for
#' joining) and measurement columns (numeric, used for diffing).
#'
#' @param x,y Data frames
#'
#' @return A list with `key_cols` and `measure_cols` character vectors.
#'
#' @noRd
classify_columns <- function(x, y) {
  common <- intersect(colnames(x), colnames(y))

  key_cols <- character()
  measure_cols <- character()

  for (col in common) {
    vals <- x[[col]]
    if (is.character(vals) || is.factor(vals) || is.logical(vals) ||
        inherits(vals, "Date") || inherits(vals, "POSIXct") ||
        (is.integer(vals) && length(unique(vals)) < 20)) {
      key_cols <- c(key_cols, col)
    } else if (is.numeric(vals)) {
      measure_cols <- c(measure_cols, col)
    }
  }

  list(key_cols = key_cols, measure_cols = measure_cols)
}
