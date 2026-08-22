# Demo board for the prose block — WYSIWYG editor, raw-markdown sync, glue chips.
# Run: Rscript dev/prose-block-demo.R   (serves on 3838)
pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.extra", quiet = TRUE)

library(shiny)

board <- new_board(
  blocks = list(
    data = new_dataset_block("mtcars", "datasets"),
    note = new_prose_block(
      "## Report\n\nThe dataset has **{nrow(data)}** rows and mean mpg of {round(mean(data$mpg), 1)}."
    )
  ),
  links = links(from = "data", to = "note", input = "data")
)

# options(shiny.port = 3838, shiny.host = "0.0.0.0")
serve(board)
