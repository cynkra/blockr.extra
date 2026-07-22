# Demo / e2e testbed for new_code_block (the CodeMirror editor).
#   Rscript blockr.extra/dev/code-block-demo.R   (serves on 3838)
# OpenAI key from /workspace/.Renviron (launch from /workspace).

pkgload::load_all("blockr.ui", quiet = TRUE)
pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dplyr", quiet = TRUE)
pkgload::load_all("blockr.ai", quiet = TRUE)
pkgload::load_all("blockr.extra")

library(shiny)

options(
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.ai_model = "gpt-5.1",
  shiny.port = 3841L
  # shiny.host = "0.0.0.0"
)

board <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars"),
    code = new_function_block()
  ),
  links = links(from = "data", to = "code")
)

print(serve(board, plugins = custom_plugins(ai_ctrl_block())))
