# Visual check for function-block field alignment (Blockr.Select + grid layout)
library(blockr)
pkgload::load_all("blockr.extra")
library(blockr.session)

blockr.core::unregister_blocks(c(
  "subset_block", "merge_block", "rbind_block", "head_block",
  "scatter_block", "upload_block", "filebrowser_block", "csv_block",
  "static_block", "glue_block"
))

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),

    # Multi-field block: single-select, multi-select, numeric, text.
    # With 4 fields the layout must never stretch a lone leftover to full width.
    many = new_function_block(
      fn = "function(data,
                      method = c('pearson', 'spearman', 'kendall'),
                      cols = list('mpg', 'cyl', 'disp', 'hp'),
                      n = 6L,
                      title = 'Result') {
        utils::head(data[, cols, drop = FALSE], n)
      }"
    ),

    # Two-field block: both fields must fill the row (50/50), no empty column.
    two = new_function_block(
      fn = "function(data, n = 6L, cols = list('mpg', 'cyl', 'disp')) {
        utils::head(data[, cols, drop = FALSE], n)
      }"
    ),

    # Single-field block: must render full width.
    one = new_function_block(
      fn = "function(data, n = 6L) { utils::head(data, n) }"
    )
  ),
  links = c(
    new_link("data", "many", "data"),
    new_link("data", "two", "data"),
    new_link("data", "one", "data")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

# Pick a port that is actually free so a stale instance can never block startup.
# Honour an explicit PORT if set; otherwise scan upward from 3838.
find_free_port <- function(start = 3838, tries = 100) {
  for (p in seq.int(start, start + tries)) {
    free <- tryCatch({
      srv <- httpuv::startServer("0.0.0.0", p, list())
      httpuv::stopServer(srv)
      TRUE
    }, error = function(e) FALSE)
    if (free) return(p)
  }
  httpuv::randomPort()
}

port <- if (nzchar(Sys.getenv("PORT"))) {
  as.integer(Sys.getenv("PORT"))
} else {
  find_free_port(3838)
}

options(shiny.port = port, shiny.host = "0.0.0.0")
message("\n>>> Field-alignment demo: http://localhost:", port, "\n")
serve(board, plugins = custom_plugins(manage_project()))
