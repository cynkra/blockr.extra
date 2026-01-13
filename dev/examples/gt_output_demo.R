# Load required libraries
library(blockr)
library(blockr.extra)
pkgload::load_all()

# Demo workflow showing GT table auto-detection
run_app(
  blocks = c(
    data = new_dataset_block(dataset = "iris"),

    # Function block that returns a GT table
    gt_summary = new_function_block(
      fn = function(data) {
        summary_df <- data |>
          dplyr::group_by(Species) |>
          dplyr::summarize(
            N = dplyr::n(),
            `Mean Sepal.L` = round(mean(Sepal.Length), 2),
            `Mean Petal.L` = round(mean(Petal.Length), 2),
            .groups = "drop"
          )

        gt::gt(summary_df) |>
          gt::tab_header(
            title = "Iris Species Summary",
            subtitle = "Measurements by species"
          ) |>
          gt::tab_style(
            style = gt::cell_fill(color = "lightblue"),
            locations = gt::cells_column_labels()
          )
      }
    )
  ),
  links = c(
    new_link("data", "gt_summary", "data")
  )
)
