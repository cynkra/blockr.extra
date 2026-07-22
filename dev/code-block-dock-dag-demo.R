# Demo / e2e testbed for new_code_block (the CodeMirror editor) on a
# blockr.dock board with the blockr.dag extension.
#
#   [data: mtcars] --> [code: function editor]
#
# Same block as code-block-demo.R, but served on a dock board (named "Editor"
# view: data table + code block side by side) plus the DAG view, with the AI
# control so you can drive AI edits and see the inline diff.
#
# From /workspace:
#   Rscript blockr.extra/dev/code-block-dock-dag-demo.R   (serves on 3838)
# OpenAI key from /workspace/.Renviron (launch from /workspace).

pkgload::load_all("blockr.ui",   quiet = TRUE)
pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.dplyr", quiet = TRUE)
pkgload::load_all("blockr.ai",   quiet = TRUE)
pkgload::load_all("blockr.dock", quiet = TRUE)
pkgload::load_all("blockr.dag",  quiet = TRUE)
pkgload::load_all("blockr.extra")

library(shiny)

options(
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.ai_model = "gpt-5.2",
  shiny.port = 3845L,
  shiny.host = "0.0.0.0"
)

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("mtcars"),
    code = new_function_block()
  ),
  links = links(from = "data", to = "code"),
  # Named PLAIN LIST of dock_layout()s (not dock_layouts()) so leaf IDs get
  # resolved; bare strings are their own panels (data | code side by side).
  layouts = list(
    Editor = dock_layout("data", "code")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

print(serve(board, plugins = custom_plugins(ai_ctrl_block())))
