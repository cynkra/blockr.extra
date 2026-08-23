#' Code Block
#'
#' A block whose state is an ordinary R script. The script transforms `data`
#' (the reserved name for the incoming data frame) into whatever the last
#' statement returns.
#'
#' @section Inputs:
#' A top-level assignment whose right-hand side is a **plain value** becomes a
#' control on the card. Everything else is code. "Plain value" means a literal,
#' or a call to `c()`, `factor()`, `as.Date()` or `as.POSIXct()`.
#'
#' ```r
#' species <- factor("setosa", unique(data$Species))   # a dropdown
#' n <- 10                                             # a number box
#' desc <- TRUE                                        # a checkbox
#'
#' data |>
#'   dplyr::filter(Species == species) |>
#'   dplyr::slice_head(n = n)
#' ```
#'
#' There is no fenced region and no marker comment. An input is a *kind of
#' line*, not a *place in the script*, so a line is classified on its own and
#' the editor paints the ones that became controls.
#'
#' A **factor** is how a select is expressed without any blockr vocabulary: its
#' levels are the choice list, and the length of its value decides single or
#' multiple. That covers a data-fed dropdown (`unique(data$site)` as levels) and
#' a fixed option list (`c("pearson", "spearman")`) with one mechanism.
#'
#' The optional `#|` annotation carries only what a value cannot say — a numeric
#' range, a nicer label, a placeholder: `n <- 10  #| number(min = 1, max = 50)`.
#'
#' @section Exported code:
#' The current values are substituted into the body as literals, so the block's
#' expression contains no assignment and the exported script is emitted bare —
#' no `local({...})` wrapper and no `.fn <- function(data, ...)` sandwich. See
#' [code-block-expr].
#'
#' @param script Character string of R code. `data` is the incoming data frame.
#' @param values Named list of current control values, restored with a saved
#'   board. Values for names the script no longer declares are dropped.
#' @param ... Additional arguments passed to [blockr.core::new_block()]
#'
#' @examples
#' blk <- new_code_block(
#'   script = "n <- 6\n\nutils::head(data, n)"
#' )
#'
#' @export
new_code_block <- function(script = "n <- 6\n\nutils::head(data, n)",
                           values = list(),
                           ...) {
  force(script)
  force(values)
  stopifnot(is.character(script), length(script) == 1L)
  stopifnot(is.list(values))

  blockr.core::new_block(
    server = function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          r_script <- as_rv(script, script)
          r_values <- shiny::reactiveVal(values)

          # Parse is cheap and pure; everything downstream derives from it.
          r_parsed <- shiny::reactive(cb_parse(r_script()))

          # Declarations are evaluated against the live upstream data, so a
          # factor's levels track the data rather than being frozen at
          # authoring time. Before data arrives they evaluate against NULL,
          # which simply yields an error spec for the ones that need it.
          # NEVER DEGRADE THE SPECS WHEN THE DATA IS MOMENTARILY GONE. `data()`
          # is not merely stale while a panel is hidden -- the visibility gate
          # makes it throw, so `dat` becomes NULL. `cb_specs()` then cannot
          # evaluate a declaration like `cyl <- factor(unique(data$cyl))`,
          # returns an error spec, and `cb_expr()` drops it as unusable: the
          # emitted expression silently loses its substitution and refers to a
          # symbol nothing defines. Coming back flips it to the good expression
          # again, so the block genuinely re-evaluated on every tab switch.
          #
          # Keeping the last good specs while the data is unavailable removes
          # that flip-flop, and the identical() guard stops an unchanged
          # recomputation from propagating. Live data still drives the choices:
          # a real upstream change gives a non-NULL, different `dat`, so the
          # specs recompute and the control updates.
          r_specs_val <- shiny::reactiveVal(NULL)

          shiny::observe({
            dat <- tryCatch(data(), error = function(e) NULL)
            prev <- shiny::isolate(r_specs_val())
            if (is.null(dat) && !is.null(prev)) {
              return()
            }
            specs <- cb_specs(r_parsed(), dat)
            if (!identical(specs, prev)) {
              r_specs_val(specs)
            }
          })

          r_specs <- shiny::reactive(r_specs_val())

          # The controls. Seeded from the current values (so a restored board
          # comes back with its knobs where the user left them) but NOT
          # reactive to them, or every turn of a knob would rebuild the UI.
          output$dynamic_params <- shiny::renderUI({
            cb_params_ui(r_specs(), ns, shiny::isolate(r_values()))
          })

          # Collect the controls back into one values list. Reading each
          # `input[[...]]` here is what makes this observer depend on them.
          shiny::observe({
            specs <- r_specs()
            vals <- shiny::isolate(r_values())
            for (s in specs) {
              if (!is.null(s$error) || is.na(s$kind)) {
                next
              }
              raw <- input[[paste0("cb_", s$name)]]
              if (is.null(raw)) {
                next
              }
              coerced <- cb_coerce(raw, s)
              if (!is.null(coerced)) {
                vals[[s$name]] <- coerced
              }
            }
            # Forget values whose declaration the script no longer has.
            declared <- vapply(specs, `[[`, character(1L), "name")
            vals <- vals[intersect(names(vals), declared)]
            if (!identical(vals, shiny::isolate(r_values()))) {
              r_values(vals)
            }
          })

          # Tell the editor which lines became controls, and what each became.
          # Driven by the *live* editor text rather than the committed script,
          # and classified syntactically (no evaluation), so a glyph appears the
          # moment you finish typing a declaration and vanishes the moment the
          # line stops being one. That is the whole convention, taught in a
          # keystroke.
          shiny::observe({
            live <- input$fn_code
            if (is.null(live)) {
              live <- r_script()
            }
            session$sendCustomMessage(
              "blockr-code-inputs",
              list(id = ns("fn_code"), marks = cb_syntactic_marks(live))
            )
          })

          # Commit a hand edit (the footer's Run button). The parse gate lives
          # in the editor layer; re-check here because this is also the path an
          # external write takes.
          shiny::observeEvent(input$submit_fn, {
            code <- input$fn_code
            if (is.null(code) || !nzchar(trimws(code))) {
              return()
            }
            if (!is.null(parse_error(code))) {
              return()
            }
            r_script(code)
          })

          setup_code_editor_server(
            input, output, session,
            base = list(r_fn_text = r_script),
            cols = shiny::reactive(
              tryCatch(names(data()), error = function(e) character())
            ),
            rest_label = shiny::reactive(cb_rest_label(r_specs(), r_parsed()))
          )

          # THE EXPRESSION MUST NOT RE-FIRE WHEN NOTHING ABOUT IT CHANGED.
          # `r_specs()` reads `data()` so that a declaration's choices track the
          # live data (see above), which means this recomputes whenever the
          # upstream data reactive churns -- including every time a hidden panel
          # is shown again, when the visibility gate re-opens. Core re-evaluates
          # the block on any firing of `expr`, without comparing, so the block
          # re-ran on every tab switch and a `Sys.time()` in a script visibly
          # changed. Holding the last expression and publishing only on a real
          # change keeps the live-data specs and stops the spurious re-runs.
          r_expr <- shiny::reactiveVal(NULL)

          shiny::observe({
            parsed <- r_parsed()
            shiny::req(parsed$ok)
            expr <- cb_expr(parsed, r_specs(), r_values())
            shiny::req(expr)
            if (!identical(expr, shiny::isolate(r_expr()))) {
              r_expr(expr)
            }
          })

          list(
            expr = shiny::reactive({
              shiny::req(r_expr())
            }),
            state = list(
              script = r_script,
              values = r_values
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        fb_select_deps(),
        shiny::div(
          class = "function-block-container",
          gear_editor_ui(ns, script, label = "R code",
                         marks = cb_syntactic_marks(script)),
          shiny::div(
            class = "function-block-params",
            shiny::uiOutput(ns("dynamic_params"))
          )
        )
      )
    },
    class = "code_block",
    expr_type = "bquoted",
    allow_empty_state = TRUE,
    external_ctrl = "script",
    ...
  )
}


#' The quiet line in the editor footer
#'
#' The explanation that does not live in the code: how many controls the script
#' produced, and the one sentence that says why.
#'
#' @noRd
cb_rest_label <- function(specs, parsed) {
  bad <- Filter(function(s) !is.null(s$error), specs)
  if (length(bad)) {
    return(paste0(bad[[1L]]$name, ": ", bad[[1L]]$error))
  }
  shadowed <- cb_shadowed(parsed)
  if (length(shadowed)) {
    return(sprintf(
      "%s assigned in the body, so %s stays code",
      paste(shadowed, collapse = ", "),
      if (length(shadowed) == 1L) "it" else "they"
    ))
  }
  n <- length(specs)
  if (!n) {
    return("no inputs · assign a plain value to get a control")
  }
  sprintf(
    "%d input%s · lines assigning a plain value become controls",
    n, if (n == 1L) "" else "s"
  )
}


#' @export
block_eval.code_block <- function(x, expr, env, ...) {
  eval_with_plot_capture(expr, env)
}

#' @export
block_ui.code_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @export
block_output.code_block <- function(x, result, session) {
  render_dynamic_output(result, x, session)
}
