# Minimal ADaM workflow: read parquet files, pull a table
#
# Requires: blockr.dm (for dm blocks + bundled ADaM parquet data)

pkgload::load_all("../blockr.core")
pkgload::load_all("../blockr.dock")
pkgload::load_all("../blockr.dm")
pkgload::load_all("../blockr.extra")

library(blockr)

options(blockr.html_table_preview = TRUE)

adam_dir <- system.file("extdata", "adam", package = "blockr.dm")

serve(
  new_dock_board(
    blocks = c(
      dm_raw = new_dm_read_block(path = adam_dir),
      dm_obj = new_dm_block(infer_keys = TRUE),
      result = new_dm_pull_block(table = "adsl")
    ),
    links = c(
      new_link("dm_raw", "dm_obj", "data"),
      new_link("dm_obj", "result", "data")
    ),
    extensions = list(dag = new_dag_extension())
  )
)
