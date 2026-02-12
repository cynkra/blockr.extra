# AI Control Example — Function Blocks
#
# Demonstrates AI control on blockr.extra function blocks.
# Each block is independently connected to a dataset block so you can
# test them one-by-one via the "AI Assist" chat overlay.
#
# Usage:
#   1. Start the app
#   2. Expand "AI Assist" on any function block
#   3. Type a natural language instruction
#
# Example prompts per block:
#   head_fn:     "show the first 3 rows"
#   summary_fn:  "filter to versicolor and compute mean sepal width"
#   plot_fn:     "scatter plot of hp vs mpg, colored by cyl"
#   model_fn:    "fit a linear model of mpg ~ wt + hp"

pkgload::load_all("../blockr.core")
pkgload::load_all("../blockr.dock")
pkgload::load_all("../blockr.dag")
pkgload::load_all("../blockr.ai")
pkgload::load_all("../blockr.extra")

options(blockr.html_table_preview = TRUE)

serve(
  new_dock_board(
    blocks = c(
      # ---- data sources ----
      iris_data   = new_dataset_block("iris"),
      mtcars_data = new_dataset_block("mtcars"),

      # ---- function blocks (single input) ----
      head_fn = new_function_block(
        fn = "function(data, n = 6L) {
          utils::head(data, n)
        }"
      ),

      summary_fn = new_function_block(
        fn = "function(data, species = c('setosa', 'versicolor', 'virginica')) {
          dplyr::filter(data, Species == species) |>
            dplyr::summarize(
              n = dplyr::n(),
              mean_sepal_length = mean(Sepal.Length),
              mean_petal_length = mean(Petal.Length)
            )
        }"
      ),

      plot_fn = new_function_block(
        fn = "function(data) {
          plot(data$mpg, data$hp,
               xlab = 'MPG', ylab = 'Horsepower',
               main = 'MPG vs HP', pch = 19)
        }"
      ),

      model_fn = new_function_block(
        fn = "function(data) {
          lm(mpg ~ wt, data = data)
        }"
      ),

      # ---- function_xy_block (two inputs) ----
      join_fn = new_function_xy_block(
        fn = "function(x, y) {
          dplyr::bind_rows(
            dplyr::mutate(x, source = 'iris'),
            dplyr::mutate(y, source = 'mtcars')
          )
        }"
      )
    ),
    links = c(
      # iris pipeline
      new_link("iris_data", "head_fn",    "data"),
      new_link("iris_data", "summary_fn", "data"),

      # mtcars pipeline
      new_link("mtcars_data", "plot_fn",  "data"),
      new_link("mtcars_data", "model_fn", "data"),

      # xy block: both data sources
      new_link("iris_data",   "join_fn", "x"),
      new_link("mtcars_data", "join_fn", "y")
    ),
    extensions = list(dag = new_dag_extension())
  ),
  plugins = custom_plugins(ai_ctrl_block())
)
