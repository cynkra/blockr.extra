# Two-step modelling demo: fit model, show diagnostics and summary

library(blockr)
pkgload::load_all()

# Enable the new HTML table preview
options(blockr.html_table_preview = TRUE)

run_app(
  blocks = c(
    # Data source
    data = new_dataset_block("mtcars"),

    # Fit linear model
    model = new_function_block(
      "function(data, response = c('mpg', 'hp', 'wt'), predictor = c('wt', 'hp', 'disp', 'cyl')) {
        formula <- as.formula(paste(response, '~', predictor))
        lm(formula, data = data)
      }"
    ),

    # Diagnostic plots (base R plot.lm)
    diagnostics = new_function_block(
      "function(data) {
        par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
        plot(data, which = 1:4, ask = FALSE)
      }"
    ),

    # Model summary as HTML table using broom
    summary_tbl = new_function_block(
      "function(data) {
        broom::tidy(data) |>
          gt::gt() |>
          gt::fmt_number(decimals = 4) |>
          gt::tab_header(title = 'Model Coefficients')
      }"
    ),

    # Model fit statistics
    fit_stats = new_function_block(
      "function(data) {
        broom::glance(data) |>
          tidyr::pivot_longer(everything(), names_to = 'Statistic', values_to = 'Value') |>
          gt::gt() |>
          gt::fmt_number(columns = 'Value', decimals = 4) |>
          gt::tab_header(title = 'Model Fit Statistics')
      }"
    )
  ),
  links = c(
    new_link("data", "model", "data"),
    new_link("model", "diagnostics", "data"),
    new_link("model", "summary_tbl", "data"),
    new_link("model", "fit_stats", "data")
  )
)
