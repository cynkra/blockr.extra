#' Shared code-editor layer for function blocks
#'
#' The reusable editor surface used by every function-block variant (and the
#' composer function block in blockr.sandbox): the `Blockr.Code` CodeMirror 6
#' widget plus the R server logic for AI-review diffs, live run, and the dirty
#' footer. It is **input-agnostic** — it edits the `fn` string; the host block
#' supplies the input wiring (`setup_function_block_server`) and the `.fn(...)`
#' call. See blockr.design/open/function-block-editor.
#'
#' @name code-editor
#' @keywords internal
NULL


#' Return the R parse-error message for a code string, or NULL if it parses.
#'
#' Used to gate the editor's Run button: only syntactically valid code may be
#' committed/run, so runtime errors (not syntax errors) are what reach the
#' normal blockr evaluation system.
#'
#' @param code Character string of R code.
#' @return `NULL` if `code` parses, otherwise the condition message (string).
#' @noRd
parse_error <- function(code) {
  if (is.null(code) || !nzchar(trimws(code))) {
    return(NULL)
  }
  tryCatch({
    parse(text = code)
    NULL
  }, error = function(e) conditionMessage(e))
}


#' Wire the code editor's server logic onto a function-block `base`
#'
#' Adds the editor's reactive machinery: pushes column-name completions, opens
#' the inline diff on external (AI) writes of `fn`, runs the reduced code on a
#' hunk reject / Ctrl-Enter, handles Accept-all / Reject-all, and renders the
#' dirty footer (`output$footer_ui`). The editor input id is `fn_code` (matching
#' `setup_function_block_server`'s Apply observer).
#'
#' @param input,output,session Shiny module objects.
#' @param base The list returned by [setup_function_block_server()]
#'   (`r_fn_text`, `r_fn`, `r_version`, ...).
#' @param cols A reactive returning a character vector of column names to offer
#'   in autocomplete (boosted above the static R/dplyr verbs). Defaults to none.
#' @return Invisibly `NULL` (used for its side effects).
#' @noRd
setup_code_editor_server <- function(input, output, session, base,
                                     cols = shiny::reactive(NULL)) {
  ns <- session$ns

  # Push the upstream column names to the editor whenever the data changes.
  shiny::observeEvent(cols(), {
    session$sendCustomMessage(
      "blockr-code-completions",
      list(id = ns("fn_code"), columns = as.list(cols()))
    )
  }, ignoreNULL = FALSE)

  # The last *executed* code, and whether we're mid AI-review (diff open).
  r_prev   <- shiny::reactiveVal(shiny::isolate(base$r_fn_text()))
  r_review <- shiny::reactiveVal(FALSE)
  # Number of unresolved diff hunks (from the client) → the "N edits" label.
  r_nedits <- shiny::reactiveVal(0L)
  # True pre-AI code, held constant across the AI's multi-write discovery loop
  # so the diff's "original" side stays the genuine starting point.
  r_review_original <- shiny::reactiveVal(NULL)
  # Guards a run we triggered ourselves (Ctrl-Enter / reject) from being
  # mistaken for an external AI write and re-opening the diff.
  r_internal <- shiny::reactiveVal(FALSE)

  # External (AI / external_ctrl) write of `fn`: it runs live (base parses
  # r_fn_text → output), and we open the inline diff for review against the
  # pre-AI original. A self-triggered run (r_internal) is skipped; so is the
  # user's own Run (where r_fn_text == input$fn_code).
  shiny::observeEvent(base$r_fn_text(), {
    new <- base$r_fn_text()
    if (shiny::isolate(r_internal())) {
      r_internal(FALSE)
      r_prev(new)
      return()
    }
    if (!identical(new, input$fn_code)) {
      if (is.null(r_review_original())) {
        r_review_original(r_prev())
      }
      session$sendCustomMessage(
        "blockr-code-set",
        list(id = ns("fn_code"), code = new, original = r_review_original())
      )
      r_review(TRUE)
    }
    r_prev(new)
  }, ignoreInit = TRUE)

  # Run-now path (Ctrl-Enter, and reject-a-hunk auto-run): commit the editor's
  # current code so it executes immediately, without opening a diff. JS has
  # already synced input$fn_code to the same value. Run is the *parse gate*:
  # un-parseable code never commits (the footer below also disables the Run
  # button for it), so only syntactically valid code ever reaches evaluation —
  # runtime errors then surface through the normal blockr error system.
  shiny::observeEvent(input$fn_exec, {
    code <- input$fn_exec
    if (is.null(code) || !nzchar(trimws(code))) {
      return()
    }
    if (!is.null(parse_error(code))) {
      return()
    }
    r_internal(TRUE)
    base$r_fn_text(code)
  })

  # Client reports the AI review is finished (all hunks resolved individually).
  shiny::observeEvent(input$fn_review_done, {
    r_review(FALSE)
    r_review_original(NULL)
  })

  # Accept all: keep the AI code (already running), just close the diff.
  shiny::observeEvent(input$accept_all, {
    session$sendCustomMessage("blockr-code-merge-clear", list(id = ns("fn_code")))
    r_review(FALSE)
    r_review_original(NULL)
  })

  # Reject all: client restores the diff's own original and runs it (via the
  # run-now path); we just close out the review state here.
  shiny::observeEvent(input$reject_all, {
    session$sendCustomMessage("blockr-code-reject-all", list(id = ns("fn_code")))
    r_review(FALSE)
    r_review_original(NULL)
  })

  # Client reports the live hunk count (drives the "N edits" label).
  shiny::observeEvent(input$fn_nedits, {
    n <- suppressWarnings(as.integer(input$fn_nedits))
    r_nedits(if (is.na(n)) 0L else n)
  })

  # A single flush footer bar: during an AI review it carries the count +
  # Accept-all / Reject-all; otherwise it shows "Run" only when the editor holds
  # unexecuted hand-edits; at rest it's absent. (Review and Run are mutually
  # exclusive — the first manual keystroke ends a review.)
  output$footer_ui <- shiny::renderUI({
    if (isTRUE(r_review())) {
      n <- r_nedits()
      label <- if (is.na(n) || n <= 0L) {
        "Review change"
      } else {
        sprintf("%d %s", n, if (n == 1L) "edit" else "edits")
      }
      code_block_footer(
        label,
        shiny::actionButton(ns("accept_all"), "Accept all",
                            class = "blockr-code-btn blockr-code-btn--accept"),
        shiny::actionButton(ns("reject_all"), "Reject all",
                            class = "blockr-code-btn blockr-code-btn--reject")
      )
    } else {
      ed <- input$fn_code
      if (is.null(ed) || identical(trimws(ed), trimws(base$r_fn_text()))) {
        return(NULL)
      }
      perr <- parse_error(ed)
      if (!is.null(perr)) {
        # Syntax error in the pending edit: running it makes no sense, so the
        # Run button is disabled and the footer turns red with the message.
        # Only a parse error is shown here — runtime errors go through the
        # normal evaluation system, not this footer.
        code_block_footer(
          shiny::span(class = "blockr-code-syntax-msg", "Syntax error"),
          shiny::actionButton(ns("submit_fn"), "Run",
                              class = "blockr-code-btn blockr-code-btn--run",
                              disabled = TRUE)
        )
      } else {
        code_block_footer(
          "Pending edits",
          shiny::actionButton(ns("submit_fn"), "Run",
                              class = "blockr-code-btn blockr-code-btn--run")
        )
      }
    }
  })

  # The footer lives inside the gear popover, which starts `display:none`; Shiny
  # would otherwise suspend it and a JS-driven open wouldn't resume it (so the
  # dirty/Run + syntax-error footer never appeared). Keep it live regardless —
  # it's hidden with its parent when the gear is closed anyway.
  shiny::outputOptions(output, "footer_ui", suspendWhenHidden = FALSE)

  invisible(NULL)
}


#' The code-editor UI: a bordered frame holding the CodeMirror mount + footer
#'
#' Input-agnostic. The host block decides where to place it (inside an
#' "Edit function" collapse, a gear popover, etc.). Carries the deps.
#'
#' @param ns Namespace function (`shiny::NS(id)`).
#' @param fn_text Initial function code (baked into `data-value`).
#' @param label Field label shown above the editor.
#' @return A Shiny tag.
#' @noRd
code_editor_ui <- function(ns, fn_text, label = "Function code") {
  shiny::tagList(
    blockr_code_dep(),
    code_block_css_dep(),
    shiny::div(
      class = "blockr-code-block",
      if (!is.null(label)) {
        shiny::tags$label(class = "blockr-code-label", label)
      },
      shiny::div(
        class = "blockr-code-frame",
        shiny::div(
          id = ns("fn_code"),
          class = "blockr-code",
          `data-value` = fn_text,
          # Event-input ids the JS uses to run-now, report hunk count, and
          # signal review completion.
          `data-run-input` = ns("fn_exec"),
          `data-review-done` = ns("fn_review_done"),
          `data-count-input` = ns("fn_nedits")
        ),
        shiny::uiOutput(ns("footer_ui"))
      )
    )
  )
}


#' A flush footer bar: a left status label and right-aligned actions.
#' @noRd
code_block_footer <- function(label, ...) {
  shiny::div(
    class = "blockr-code-footer",
    shiny::span(class = "blockr-code-footer__label", label),
    shiny::span(class = "blockr-code-footer__actions", ...)
  )
}


#' JS to re-measure a hidden CodeMirror editor when its container is revealed.
#'
#' CodeMirror mis-measures when mounted inside a `display:none` / collapsed
#' container; call this on the reveal toggle so it lays out correctly.
#'
#' @param fn_code_id The namespaced id of the `.blockr-code` mount
#'   (i.e. `ns("fn_code")`).
#' @return A JS expression string.
#' @noRd
code_editor_refresh_js <- function(fn_code_id) {
  sprintf(
    "if (window.Blockr && window.Blockr.Code) { setTimeout(function(){ window.Blockr.Code.refresh('%s'); }, 50); }",
    fn_code_id
  )
}
