# Load required libraries
library(blockr)
library(blockr.extra)
pkgload::load_all()

# Demo workflow for function_xy_block (joining two data frames)
run_app(
  blocks = c(
    # First data source
    orders = new_dataset_block(dataset = "mtcars"),

    # Create a second dataset by filtering
    cars_4cyl = new_function_block(
      fn = function(data) {
        data.frame(
          name = rownames(data)[data$cyl == 4],
          category = "economy"
        )
      }
    ),

    cars_8cyl = new_function_block(
      fn = function(data) {
        data.frame(
          name = rownames(data)[data$cyl == 8],
          category = "performance"
        )
      }
    ),

    # Join the two filtered datasets
    combined = new_function_xy_block(
      fn = function(x, y) {
        dplyr::bind_rows(x, y)
      }
    )
  ),
  links = c(
    new_link("orders", "cars_4cyl", "data"),
    new_link("orders", "cars_8cyl", "data"),
    new_link("cars_4cyl", "combined", "x"),
    new_link("cars_8cyl", "combined", "y")
  )
)
