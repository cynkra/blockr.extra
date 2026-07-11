# Cross-block external control: drill chart -> ctrl_send() -> patient profile.
#
# The lazy-eval-safe replacement for the latest-block fan-in. A lab ALT
# trajectory chart drills per patient (drill = "USUBJID"). Its click-filtered
# frame feeds a small function block whose fn calls blockr.extra::ctrl_send():
# when the frame has narrowed to exactly one subject, the block pushes that
# USUBJID into the patient-profile block's externally controllable `subject`
# argument through the board's regular update channel. There is NO data link
# from the chart to the profile -- the profile's only upstream is the cdisc dm
# -- so under lazy eval a profile view never drags chart pipelines in, and
# under eager eval nothing changes.
#
#   data (safetyData ADaM) -> cdisc -> flat (adlbc) -> alt (PARAMCD == ALT)
#                               |          -> traj (drill USUBJID) -> send
#                               `-> profile          (send --ctrl--> profile)
#
# The channel is installed by a no-UI dock extension (extensions receive the
# board `update` reactive; the dock board's `callbacks` slot is occupied by
# its visibility reporter).
#
# Things to try:
#   - Lab view: click a patient's trajectory point. The send block's preview
#     flips to sent = TRUE and the profile jumps to that patient.
#   - Click another patient: profile follows (latest write wins).
#   - Pipeline view: chart and profile are separate strands -- the control
#     edge is not a data link.
#
# Run from the workspace root (or from the package dir):
#   Rscript blockr.extra/dev/preview-ctrl-send-profile.R          # port 3838
#   Rscript blockr.extra/dev/preview-ctrl-send-profile.R 3839     # any port
#   BLOCKR_PORT=3839 Rscript blockr.extra/dev/preview-ctrl-send-profile.R
#
# NOTE: load_all() ALL of them, never a mix (theme asset resolution).

root <- if (file.exists("blockr.extra/DESCRIPTION")) "." else ".."
for (p in c("blockr.core", "blockr.theme", "blockr.dplyr", "blockr.dm",
            "blockr.pharma", "blockr.viz", "blockr.extra", "blockr.dock",
            # source blockr.dag, NOT the installed one: the installed 0.1.2
            # predates dock's bundled-`extensions` action contract (its action
            # factories still want a bare `dag_extension` arg -> startup error)
            "blockr.dag")) {
  pkgload::load_all(file.path(root, p), quiet = TRUE)
}

# Supplies the ADaM example tables (adsl/adae/adlbc/advs) behind the
# "safetydata_adam" choice of the dm example block.
library(safetyData)

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

options(
  shiny.port = port,
  "g6R.preserve_elements_position" = TRUE
)
message("ctrl_send demo on http://127.0.0.1:", port, "/")

# No-UI extension whose only job is to expose the board update channel to
# block code (blockr.extra::ctrl_send). Wrapped in a constructor so
# `new_dock_extension()`'s ctor resolution has a named caller.
new_ctrl_bridge_extension <- function() {
  new_dock_extension(
    server = function(id, board, update, ...) {
      shiny::moduleServer(id, function(input, output, session) {
        blockr.extra::install_ctrl_send(update)
        list(state = list())
      })
    },
    ui = function(ns, ...) htmltools::div(),
    name = "Control bridge",
    class = "ctrl_bridge_extension"
  )
}

serve(
  new_dock_board(
    blocks = c(
      data = new_dm_example_block(dataset = "safetydata_adam"),
      cdisc = new_cdisc_dm_block(),
      flat = new_dm_flatten_block(start_table = "adlbc"),
      alt = new_filter_block(
        conditions = list(
          list(type = "values", column = "PARAMCD",
               values = list("ALT"), mode = "include")
        )
      ),
      traj = new_chart_block(
        chart_type = "line",
        x = "ADY", y = "AVAL",
        series = "USUBJID",
        drill = "USUBJID",
        block_name = "ALT — Individual trajectories"
      ),
      send = new_function_block(
        fn = "function(data) {
  ids <- unique(data$USUBJID)
  if (length(ids) == 1L) {
    blockr.extra::ctrl_send('profile', subject = ids)
  }
  data.frame(subjects_in_view = length(ids), sent = length(ids) == 1L)
}",
        block_name = "Send patient to profile"
      ),
      profile = new_patient_profile_block(
        selected = c("patient_overview", "ae_gantt", "liver_panel"),
        block_name = "Patient profile"
      )
    ),
    links = list(
      list(from = "data", to = "cdisc", input = "data"),
      list(from = "cdisc", to = "flat", input = "data"),
      list(from = "flat", to = "alt", input = "data"),
      list(from = "alt", to = "traj", input = "data"),
      list(from = "traj", to = "send", input = "data"),
      list(from = "cdisc", to = "profile", input = "data")
    ),
    extensions = list(
      dag_extension = new_dag_extension(),
      ctrl_bridge = new_ctrl_bridge_extension()
    ),
    grids = list(
      Lab = dock_grid(list("traj", "send"), "profile", sizes = c(1, 1)),
      Pipeline = dock_grid("dag_extension"),
      Data = dock_grid(c("data", "cdisc", "flat", "alt"))
    ),
    active = "Lab"
  )
)
