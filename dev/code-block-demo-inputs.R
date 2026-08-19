# Demo / e2e testbed for new_code_block: a plain R script whose top-level
# assignments of plain values become controls.
#
#   [data: iris] --> [code: script with a factor, a number and a checkbox]
#
# What to look at:
#   * open the gear: the declaration lines carry a tinted band and a gutter
#     glyph naming the widget (# number, Aa text, ☑ checkbox, ▾ select)
#   * type a new `x <- 5` inside the script and watch the glyph appear, then
#     change it to `x <- nrow(data)` and watch it go
#   * the footer at rest says how many controls the script produced
#   * the DAG view's code export has NO local() wrapper and the current knob
#     values are written into the code as literals
#
# From /workspace:
#   Rscript blockr.extra/dev/code-block-demo-inputs.R

pkgload::load_all("blockr.ui",    quiet = TRUE)
pkgload::load_all("blockr.core",  quiet = TRUE)
pkgload::load_all("blockr.dplyr", quiet = TRUE)
pkgload::load_all("blockr.ai",    quiet = TRUE)
pkgload::load_all("blockr.dock",  quiet = TRUE)
pkgload::load_all("blockr.dag",   quiet = TRUE)
pkgload::load_all("blockr.extra")

library(shiny)

port <- blockr_port()

options(
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.ai_model = "gpt-5.1",
  shiny.port = port,
  shiny.host = "0.0.0.0"
)

script <- paste(
  'species <- factor(c("setosa", "versicolor"), unique(data$Species))',
  'n <- 10            #| number(min = 1, max = 50, label = "Rows")',
  'desc <- TRUE',
  '',
  'data |>',
  '  dplyr::filter(Species %in% species) |>',
  '  dplyr::arrange(if (desc) dplyr::desc(Sepal.Length) else Sepal.Length) |>',
  '  dplyr::slice_head(n = n)',
  sep = "\n"
)

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris"),
    code = new_code_block(script = script)
  ),
  links = links(from = "data", to = "code"),
  # dock_layout()/layouts= are gone since the view/grid refactor; a leftover
  # `layouts =` is silently swallowed into `...` rather than erroring.
  grids = list(
    Editor = dock_grid("data", "code")
  ),
  extensions = new_dock_extensions(list(
    new_dag_extension()
  ))
)

message("\n  http://127.0.0.1:", port, "/\n")
print(serve(board, plugins = custom_plugins(ai_ctrl_block())))
