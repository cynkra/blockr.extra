# Minimal ADaM workflow: read parquet files, pull a table
#
# Requires: blockr.dm (for dm blocks + bundled ADaM parquet data)

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.extra")

library(blockr)

options(blockr.tabular_display = blockr.ui::html_table_display)

adam_dir <- system.file("extdata", "adam", package = "blockr.dm")

serve(
  new_dock_board(
    blocks = c(
      dm_raw = new_dm_example_block()    ),
    extensions = list(dag = new_dag_extension())
  )
)
