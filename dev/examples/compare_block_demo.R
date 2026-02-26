# Compare block demo: diff two data frames on shared keys
# Uses dock board layout with DAG visualization

library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.extra)
pkgload::load_all("blockr.extra")

options(shiny.port = 7860, shiny.launch.browser = FALSE)

# --- Sample data: insurance pricing from two models ---
baseline <- data.frame(
  region = c("US", "EU", "APAC", "LATAM"),
  layer = c("Primary", "Primary", "Excess", "Excess"),
  risk_premium = c(1200, 980, 450, 670),
  model_price = c(1350, 1100, 510, 740)
)

revised <- data.frame(
  region = c("US", "EU", "APAC", "LATAM"),
  layer = c("Primary", "Primary", "Excess", "Excess"),
  risk_premium = c(1260, 950, 480, 670),
  model_price = c(1400, 1050, 530, 720)
)

serve(
  new_dock_board(
    blocks = c(
      base = new_static_block(baseline),
      rev = new_static_block(revised),
      cmp = new_compare_block()
    ),
    links = list(
      from = c("base", "rev"),
      to = c("cmp", "cmp"),
      input = c("x", "y")
    ),
    stacks = c(
      inputs = new_dock_stack(c("base", "rev")),
      output = new_dock_stack("cmp")
    ),
    extensions = new_dag_extension()
  )
)
