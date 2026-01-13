# Load required libraries
library(blockr)
library(blockr.extra)
pkgload::load_all()

# Demo workflow for function_var_block (combining multiple data frames)
run_app(
  blocks = c(
    # Three separate data sources
    iris_data = new_dataset_block(dataset = "iris"),
    mtcars_data = new_dataset_block(dataset = "mtcars"),

    # Filter to create different subsets
    setosa = new_function_block(
      fn = function(data) {
        dplyr::filter(data, Species == "setosa") |>
          dplyr::select(Sepal.Length, Petal.Length) |>
          dplyr::mutate(source = "setosa")
      }
    ),

    versicolor = new_function_block(
      fn = function(data) {
        dplyr::filter(data, Species == "versicolor") |>
          dplyr::select(Sepal.Length, Petal.Length) |>
          dplyr::mutate(source = "versicolor")
      }
    ),

    virginica = new_function_block(
      fn = function(data) {
        dplyr::filter(data, Species == "virginica") |>
          dplyr::select(Sepal.Length, Petal.Length) |>
          dplyr::mutate(source = "virginica")
      }
    ),

    # Combine all three using variadic function block
    all_species = new_function_var_block(
      fn = function(...) {
        dplyr::bind_rows(...)
      }
    )
  ),
  links = c(
    new_link("iris_data", "setosa", "data"),
    new_link("iris_data", "versicolor", "data"),
    new_link("iris_data", "virginica", "data"),
    new_link("setosa", "all_species", "...1"),
    new_link("versicolor", "all_species", "...2"),
    new_link("virginica", "all_species", "...3")
  )
)
