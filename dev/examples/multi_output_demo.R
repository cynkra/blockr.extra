# Multi-output support demo - all 5 output types in ONE workflow

library(blockr)
pkgload::load_all()

run_app(
  blocks = c(
    data = new_dataset_block("iris"),
    gt_table = new_function_block("function(data) { gt::gt(head(data, 5)) }"),
    ggplot = new_function_block("function(data) { ggplot2::ggplot(data, ggplot2::aes(Sepal.Length, Sepal.Width, color = Species)) + ggplot2::geom_point() }"),
    base_plot = new_function_block("function(data) { plot(data$Sepal.Length, data$Sepal.Width, col = as.numeric(data$Species), pch = 19) }"),
    dataframe = new_function_block("function(data) { head(data, 10) }"),
    model = new_function_block("function(data) { lm(Sepal.Width ~ Sepal.Length, data) }")
  ),
  links = c(
    new_link("data", "gt_table", "data"),
    new_link("data", "ggplot", "data"),
    new_link("data", "base_plot", "data"),
    new_link("data", "dataframe", "data"),
    new_link("data", "model", "data")
  )
)
