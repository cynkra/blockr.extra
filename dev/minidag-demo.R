# Demo / e2e testbed for the minidag extension: a blockr.dock board that uses
# minidag INSTEAD of the blockr.dag deck to configure the workflow.
#
# The block set exercises every input arity the deck distinguishes:
#
#   d1 (dataset, 0 inputs) --> f1 (filter, 1 input) --> h1 (head, 1 input)
#   d1 + f1                --> m1 (merge, 2 named inputs x / y)
#   h1 + f1                --> r1 (rbind, variadic)
#
# From /workspace:
#   Rscript blockr.extra/dev/minidag-demo.R   (serves on http://127.0.0.1:3838)
#   PORT=3839 Rscript blockr.extra/dev/minidag-demo.R   (any other port)

pkgload::load_all("blockr.ui", quiet = TRUE)
pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.dplyr", quiet = TRUE)
pkgload::load_all("blockr.dock", quiet = TRUE)
pkgload::load_all("blockr.extra")

library(shiny)

port <- as.integer(Sys.getenv("PORT", "3838"))

options(
  blockr.tabular_display = blockr.ui::html_table_display,
  shiny.port = port,
  shiny.host = "0.0.0.0"
)

board <- new_dock_board(
  blocks = c(
    d1 = new_dataset_block("iris", block_name = "Iris data"),
    f1 = blockr.dplyr::new_filter_block(
      conditions = list(
        list(
          type = "values",
          column = "Species",
          values = list("virginica"),
          mode = "exclude"
        )
      ),
      block_name = "Two species"
    ),
    h1 = new_head_block(n = 6L, block_name = "First rows"),
    m1 = new_merge_block(by = "Species", block_name = "Self merge"),
    r1 = new_rbind_block(block_name = "Bind rows")
  ),
  links = links(
    from = c("d1", "f1", "d1", "f1", "h1", "f1"),
    to = c("f1", "h1", "m1", "m1", "r1", "r1"),
    input = c("data", "data", "x", "y", "", "")
  ),
  stacks = stacks(
    prep = new_dock_stack(
      c("d1", "f1"),
      name = "Data prep",
      color = "#2563eb"
    )
  ),
  extensions = list(
    new_minidag_extension()
  )
)

cat(sprintf("\nMinidag demo: http://127.0.0.1:%d/\n\n", port))

serve(board)
