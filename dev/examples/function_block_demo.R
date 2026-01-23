# Load required libraries
library(blockr)
# library(blockr.extra)
pkgload::load_all()

library(blockr.session)

options(
  "g6R.layout_on_data_change" = TRUE
)

# Unregister all blockr.core blocks except dataset_block
# (other packages like blockr.io, blockr.dplyr, blockr.ggplot provide better alternatives)
blockr.core::unregister_blocks(c(
  "subset_block",
  "merge_block",
  "rbind_block",
  "head_block",
  "scatter_block",
  "upload_block",
  "filebrowser_block",
  "csv_block",
  "static_block",
  "glue_block"
))



# Demo workflow for function_block
board <- new_dock_board(
  blocks = c(
    data = new_dataset_block(dataset = "iris"),

    # Function block with custom head function
    head_fn = new_function_block(
      fn = "function(data, n = 10L) {
        utils::head(data, n)
      }"
    ),

    # Function block that filters and summarizes
    summary_fn = new_function_block(
      fn = "function(data, species = c('setosa', 'versicolor', 'virginica')) {
        dplyr::filter(data, Species == species) |>
          dplyr::summarize(
            n = dplyr::n(),
            mean_sepal_length = mean(Sepal.Length),
            mean_petal_length = mean(Petal.Length)
          )
      }"
    )
  ),
  links = c(
    new_link("data", "head_fn", "data"),
    new_link("data", "summary_fn", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)


# Serve with manage_project plugin for workflow save/load
serve(board, plugins = custom_plugins(manage_project()))
