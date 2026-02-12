# Two-step modelling demo: fit model, show diagnostics and summary

library(blockr)
pkgload::load_all()
pkgload::load_all("../blockr.extra")

# Enable the new HTML table preview
options(blockr.html_table_preview = TRUE)

run_app(
  blocks = c(
    # Data source
    data = new_dataset_block("mtcars"),

    # Fit linear model - named list() for multi-select with labels
    model = new_function_block(
      "function(data, response = c('MPG' = 'mpg', 'Horsepower' = 'hp', 'Weight' = 'wt'), predictors = list('Weight' = 'wt', 'Horsepower' = 'hp', 'Displacement' = 'disp', 'Cylinders' = 'cyl')) {
        formula <- as.formula(paste(response, '~', paste(predictors, collapse = ' + ')))
        lm(formula, data = data)
      }"
    ),

    # Diagnostic plot - named c() maps labels to values
    # Note: as.integer() needed because Shiny returns selectInput values as character
    diagnostics = new_function_block(
      "function(data, diagnostic = c('Residuals vs Fitted' = 1, 'Normal Q-Q' = 2, 'Scale-Location' = 3, 'Cooks Distance' = 4, 'Residuals vs Leverage' = 5, 'Cooks vs Leverage' = 6)) {
        plot(data, which = as.integer(diagnostic))
      }"
    ),

    # Model summary using gtsummary
    summary_tbl = new_function_block(
      "function(data) {
        gtsummary::tbl_regression(data) |>
          gtsummary::as_gt()
      }"
    ),

    # Model fit statistics using broom
    fit_stats = new_function_block(
      "function(data) {
        broom::glance(data) |>
          tidyr::pivot_longer(tidyr::everything(), names_to = 'Statistic', values_to = 'Value') |>
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
