# Load required libraries
library(blockr)
library(blockr.extra)
pkgload::load_all()

# Demo workflow for function_block
run_app(
  blocks = c(
    data = new_dataset_block(dataset = "iris"),

    # Function block with custom head function
    head_fn = new_function_block(
      fn = function(data, n = 10L) {
        utils::head(data, n)
      }
    ),

    # Function block that filters and summarizes
    summary_fn = new_function_block(
      fn = function(data, species = c("setosa", "versicolor", "virginica")) {
        dplyr::filter(data, Species == species) |>
          dplyr::summarize(
            n = dplyr::n(),
            mean_sepal_length = mean(Sepal.Length),
            mean_petal_length = mean(Petal.Length)
          )
      }
    )
  ),
  links = c(
    new_link("data", "head_fn", "data"),
    new_link("data", "summary_fn", "data")
  )
)
