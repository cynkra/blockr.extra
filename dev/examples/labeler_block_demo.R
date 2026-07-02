# Labeler block demo — edit column labels (attr(col, "label")).
#
# mtcars has no labels; the labeler adds them. Downstream, the rename
# block's column picker shows the labels as secondary text, demonstrating
# that the attribute flows through the pipeline.

pkgload::load_all("blockr.extra", export_all = FALSE)

blockr.core::serve(
  blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("mtcars"),
      labeler = blockr.extra::new_labeler_block(
        labels = list(mpg = "Miles per gallon")
      ),
      rename = blockr.dplyr::new_rename_block()
    ),
    links = c(
      blockr.core::new_link("data", "labeler", "data"),
      blockr.core::new_link("labeler", "rename", "data")
    )
  ),
  port = 3838,
  host = "0.0.0.0"
)
