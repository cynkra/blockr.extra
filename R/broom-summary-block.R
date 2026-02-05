#' Broom Summary Block
#'
#' Creates an adaptive HTML summary display for ANY broom-compatible model.
#' Shows coefficients, model statistics, and influence diagnostics in a
#' responsive column layout that adapts based on which broom methods work.
#'
#' Works with lm, glm, lmer, rlm, coxph, gam, and 100+ other model types
#' supported by broom.
#'
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block object of class \code{broom_summary_block}.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#'   serve(new_broom_summary_block(), list(data = model))
#' }
#'
#' @importFrom shiny moduleServer reactive tagList tags div uiOutput renderUI NS
#' @export
new_broom_summary_block <- function(...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              # Return the model with summary info as attributes
              parse(
                text = "{
                  # Safe broom functions
                  safe_tidy <- function(model) {
                    tryCatch(broom::tidy(model, conf.int = TRUE), error = function(e) NULL)
                  }
                  safe_glance <- function(model) {
                    tryCatch(broom::glance(model), error = function(e) NULL)
                  }
                  safe_augment <- function(model) {
                    tryCatch(broom::augment(model), error = function(e) NULL)
                  }

                  tidy_result <- safe_tidy(data)
                  glance_result <- safe_glance(data)
                  augment_result <- safe_augment(data)

                  # Use glance as base if available, otherwise create minimal df
                  if (!is.null(glance_result)) {
                    df <- glance_result
                  } else if (!is.null(tidy_result)) {
                    df <- tidy_result
                  } else {
                    df <- data.frame(model_class = class(data)[1])
                  }

                  attr(df, \"model\") <- data
                  attr(df, \"tidy\") <- tidy_result
                  attr(df, \"glance\") <- glance_result
                  attr(df, \"augment\") <- augment_result
                  df
                }"
              )[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      shiny::tagList() # Empty UI, custom display handled by block_ui method
    },
    class = "broom_summary_block",
    ...
  )
}

#' Generate significance badge for broom summary
#' @param p p-value (can be NA)
#' @param conf_low lower bound of confidence interval (optional)
#' @param conf_high upper bound of confidence interval (optional)
#' @noRd
broom_significance_badge <- function(p = NA, conf_low = NA, conf_high = NA) {
  # If p-value available, use it

if (!is.na(p)) {
    if (p < 0.001) {
      return(tags$span(
        class = "badge",
        style = "background-color: #0066cc;",
        "0.1%"
      ))
    } else if (p < 0.01) {
      return(tags$span(
        class = "badge",
        style = "background-color: #17a2b8;",
        "1%"
      ))
    } else if (p < 0.05) {
      return(tags$span(
        class = "badge",
        style = "background-color: #6c757d;",
        "5%"
      ))
    } else if (p < 0.1) {
      return(tags$span(
        class = "badge",
        style = "background-color: #6c757d;",
        "10%"
      ))
    } else {
      return("")
    }
  }

  # Fallback: use confidence interval if available
  # 95% CI excludes 0 → significant at 5% level
  if (!is.na(conf_low) && !is.na(conf_high)) {
    excludes_zero <- (conf_low > 0 && conf_high > 0) || (conf_low < 0 && conf_high < 0)
    if (excludes_zero) {
      return(tags$span(
        class = "badge",
        style = "background-color: #6c757d;",
        "5%"
      ))
    }
  }

""
}

#' Generate HTML coefficients table from tidy output
#' @noRd
broom_html_coefs <- function(tidy_df) {
  if (is.null(tidy_df) || nrow(tidy_df) == 0) {
    return(NULL)
  }

  tags$table(
    class = "table table-condensed",
    style = "width: 100%; font-size: 13px;",
    tags$tbody(
      lapply(seq_len(nrow(tidy_df)), function(i) {
        row <- tidy_df[i, ]
        p_val <- if ("p.value" %in% names(row)) row$p.value else NA
        conf_low <- if ("conf.low" %in% names(row)) row$conf.low else NA
        conf_high <- if ("conf.high" %in% names(row)) row$conf.high else NA

        tags$tr(
          tags$td(
            style = "padding: 4px 8px 4px 0; font-weight: 500;",
            row$term
          ),
          tags$td(
            style = "padding: 4px 8px; text-align: right;",
            sprintf("%.2f", row$estimate)
          ),
          tags$td(
            style = "padding: 4px 0 4px 8px; text-align: right;",
            broom_significance_badge(p_val, conf_low, conf_high)
          )
        )
      })
    )
  )
}

#' Generate HTML statistics table from glance output
#' @noRd
broom_html_stats <- function(glance_df, model) {
  # Build stats items based on what's available
  stats_items <- list()

  # Model type
  model_class <- class(model)[1]
  stats_items <- c(stats_items, list(c("Model", model_class)))

  if (!is.null(glance_df) && nrow(glance_df) > 0) {
    # Common statistics
    if ("nobs" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("Observations", glance_df$nobs)))
    }

    if ("r.squared" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("R\u00b2", sprintf("%.4f", glance_df$r.squared))))
    }

    if ("adj.r.squared" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("Adj. R\u00b2", sprintf("%.4f", glance_df$adj.r.squared))))
    }

    if ("sigma" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("Residual SE", sprintf("%.4f", glance_df$sigma))))
    }

    if ("AIC" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("AIC", sprintf("%.1f", glance_df$AIC))))
    }

    if ("BIC" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("BIC", sprintf("%.1f", glance_df$BIC))))
    }

    if ("statistic" %in% names(glance_df) && "p.value" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("F-statistic", sprintf("%.2f", glance_df$statistic))))
    }

    if ("df" %in% names(glance_df) && "df.residual" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("DF", paste0(glance_df$df, " / ", glance_df$df.residual))))
    }

    # GLM-specific
    if ("deviance" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("Deviance", sprintf("%.2f", glance_df$deviance))))
    }

    if ("null.deviance" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("Null Deviance", sprintf("%.2f", glance_df$null.deviance))))
    }

    # Mixed models
    if ("logLik" %in% names(glance_df)) {
      stats_items <- c(stats_items, list(c("Log-Lik", sprintf("%.2f", glance_df$logLik))))
    }
  }

  if (length(stats_items) == 0) {
    return(NULL)
  }

  tags$table(
    class = "table table-condensed",
    style = "width: 100%; font-size: 13px;",
    tags$tbody(
      lapply(stats_items, function(item) {
        tags$tr(
          tags$td(
            style = "padding: 4px 8px 4px 0; font-weight: 500;",
            item[1]
          ),
          tags$td(
            style = "padding: 4px 0 4px 8px; text-align: right;",
            item[2]
          )
        )
      })
    )
  )
}

#' Generate HTML influence diagnostics from augment output
#' @noRd
broom_html_influence <- function(augment_df, glance_df) {
  if (is.null(augment_df) || nrow(augment_df) == 0) {
    return(NULL)
  }

  n <- nrow(augment_df)
  influence_items <- list()

  # Number of observations
  influence_items <- c(influence_items, list(c("Observations", as.character(n))))

  # Outliers: |.std.resid| > 2
  if (".std.resid" %in% names(augment_df)) {
    outliers <- sum(abs(augment_df$.std.resid) > 2, na.rm = TRUE)
    pct <- sprintf("%.1f%%", 100 * outliers / n)
    influence_items <- c(influence_items, list(c(
      "Outliers (|std.resid| > 2)",
      paste0(outliers, " (", pct, ")")
    )))
  }

  # Influential points: Cook's D > 4/n
  if (".cooksd" %in% names(augment_df)) {
    threshold <- 4 / n
    influential <- sum(augment_df$.cooksd > threshold, na.rm = TRUE)
    pct <- sprintf("%.1f%%", 100 * influential / n)
    influence_items <- c(influence_items, list(c(
      "Influential (Cook's D > 4/n)",
      paste0(influential, " (", pct, ")")
    )))
  }

  # High leverage: .hat > 2*p/n
  if (".hat" %in% names(augment_df)) {
    # Try to get p from glance or estimate from augment
    p <- NULL
    if (!is.null(glance_df) && "df" %in% names(glance_df)) {
      p <- glance_df$df
    }
    if (is.null(p)) {
      # Estimate p as number of non-NA columns that look like predictors
      p <- 2  # Default assumption
    }
    threshold <- 2 * p / n
    high_lev <- sum(augment_df$.hat > threshold, na.rm = TRUE)
    pct <- sprintf("%.1f%%", 100 * high_lev / n)
    influence_items <- c(influence_items, list(c(
      paste0("High Leverage (.hat > 2p/n)"),
      paste0(high_lev, " (", pct, ")")
    )))
  }

  # Residual range
  if (".resid" %in% names(augment_df)) {
    resid_range <- range(augment_df$.resid, na.rm = TRUE)
    influence_items <- c(influence_items, list(c(
      "Residual Range",
      sprintf("[%.2f, %.2f]", resid_range[1], resid_range[2])
    )))
  }

  if (length(influence_items) <= 1) {
    return(NULL)
  }

  tags$table(
    class = "table table-condensed",
    style = "width: 100%; font-size: 13px;",
    tags$tbody(
      lapply(influence_items, function(item) {
        tags$tr(
          tags$td(
            style = "padding: 4px 8px 4px 0; font-weight: 500;",
            item[1]
          ),
          tags$td(
            style = "padding: 4px 0 4px 8px; text-align: right;",
            item[2]
          )
        )
      })
    )
  )
}

#' Generate full HTML summary for broom-compatible model
#' @noRd
broom_html_summary <- function(model, tidy_df, glance_df, augment_df) {
  if (is.null(model)) {
    return(div(
      style = "padding: 20px; text-align: center; color: #6c757d;",
      "No model available"
    ))
  }

  # Build sections that are available
  coefs_html <- broom_html_coefs(tidy_df)
  stats_html <- broom_html_stats(glance_df, model)
  influence_html <- broom_html_influence(augment_df, glance_df)

  # Count available sections
  sections <- list()
  if (!is.null(coefs_html)) {
    sections <- c(sections, list(list(header = "Coefficients", content = coefs_html)))
  }
  if (!is.null(stats_html)) {
    sections <- c(sections, list(list(header = "Statistics", content = stats_html)))
  }
  if (!is.null(influence_html)) {
    sections <- c(sections, list(list(header = "Influence", content = influence_html)))
  }

  n_sections <- length(sections)

  if (n_sections == 0) {
    return(div(
      style = "padding: 20px; text-align: center; color: #6c757d;",
      paste0("No broom methods available for ", class(model)[1])
    ))
  }

  # Determine column width based on number of sections
  col_class <- switch(
    as.character(n_sections),
    "1" = "col-md-12",
    "2" = "col-md-6",
    "3" = "col-md-4"
  )

  tagList(
    # CSS styling
    tags$style(HTML("
      .broom-summary-container {
        padding: 10px 0;
        background: #ffffff;
      }
      .broom-summary-container .row {
        margin-left: 0;
        margin-right: 0;
      }
      .broom-summary-container .col-md-4,
      .broom-summary-container .col-md-6,
      .broom-summary-container .col-md-12 {
        padding-left: 0;
        padding-right: 30px;
      }
      .broom-column-header {
        font-size: 14px;
        font-weight: 600;
        color: #495057;
        margin-bottom: 15px;
        padding-bottom: 8px;
        border-bottom: 1px solid #e9ecef;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .broom-summary-container .table-condensed {
        border-collapse: collapse;
        width: 100%;
      }
      .broom-summary-container .table-condensed td,
      .broom-summary-container .table-condensed th {
        border-bottom: 1px solid #f0f0f0;
        transition: background-color 0.15s;
      }
      .broom-summary-container .table-condensed td:first-child {
        padding-left: 0 !important;
      }
      .broom-summary-container .table-condensed tr:hover td {
        background-color: #f8f9fa;
      }
      .broom-summary-container .table-condensed tr:last-child td {
        border-bottom: none;
      }
      .broom-summary-container .badge {
        display: inline-block;
        padding: 3px 7px;
        font-size: 10px;
        font-weight: 600;
        line-height: 1;
        color: #fff;
        text-align: center;
        white-space: nowrap;
        vertical-align: baseline;
        border-radius: 12px;
        letter-spacing: 0.3px;
      }
    ")),

    div(
      class = "broom-summary-container",

      # Adaptive column layout
      tags$div(
        class = "row",
        lapply(sections, function(sec) {
          tags$div(
            class = col_class,
            div(class = "broom-column-header", sec$header),
            sec$content
          )
        })
      )
    )
  )
}

#' @export
block_ui.broom_summary_block <- function(id, x, ...) {
  uiOutput(NS(id, "result"))
}

#' @export
block_output.broom_summary_block <- function(x, result, session) {
  model <- attr(result, "model")
  tidy_df <- attr(result, "tidy")
  glance_df <- attr(result, "glance")
  augment_df <- attr(result, "augment")

  renderUI({
    broom_html_summary(model, tidy_df, glance_df, augment_df)
  })
}
