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

#' Condense a `parse()` error message for the status bar.
#'
#' `parse(text=)` reports e.g. `"<text>:6:0: unexpected end of input\n5: ...\n^"`.
#' We keep just the first line and reformat the `<text>:L:C:` prefix to a terse
#' `"Line L: <reason>"`. Falls back to the trimmed first line if it doesn't match.
#'
#' @param msg The string returned by [parse_error()].
#' @return A short single-line string suitable for the footer.
#' @noRd
parse_error_brief <- function(msg) {
  first <- sub("\n.*$", "", msg)
  m <- regmatches(first, regexec("^<text>:(\\d+):\\d+:\\s*(.*)$", first))[[1]]
  if (length(m) == 3L) {
    sprintf("Line %s: %s", m[2L], m[3L])
  } else {
    trimws(first)
  }
}


#' Check parsed code satisfies the block's argument contract.
#'
#' A function block's code must be a single `function(...)` literal whose leading
#' formals match `required_args` (e.g. `"data"`, or `c("x", "y")`). The check is
#' done on the parse tree — no `eval()` — so half-typed code is never executed.
#'
#' @param code Character string of R code (already known to parse).
#' @param required_args Character vector of required leading formal names, or
#'   `NULL`/empty to skip the check.
#' @param message Block-specific message to return on a violation.
#' @return `NULL` if the contract holds, otherwise `message`.
#' @noRd
contract_error <- function(code, required_args, message) {
  if (is.null(required_args) || !length(required_args)) {
    return(NULL)
  }
  exprs <- tryCatch(parse(text = code), error = function(e) NULL)
  # Must be exactly one top-level expression, and it must be a `function` literal.
  if (is.null(exprs) || length(exprs) != 1L) {
    return(message)
  }
  e <- exprs[[1L]]
  if (!is.call(e) || !identical(e[[1L]], as.name("function"))) {
    return(message)
  }
  formal_names <- names(e[[2L]])  # formals pairlist; no evaluation
  if (length(formal_names) < length(required_args) ||
      !identical(formal_names[seq_along(required_args)], required_args)) {
    return(message)
  }
  NULL
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
#' @param required_args Character vector of required leading formal names for the
#'   block's signature contract (e.g. `"data"`), surfaced in the status bar when
#'   violated. `NULL` skips the contract check.
#' @param contract_message Message shown in the status bar when `required_args`
#'   is violated (the block's existing `error_message`).
#' @return Invisibly `NULL` (used for its side effects).
#' @noRd
setup_code_editor_server <- function(input, output, session, base,
                                     cols = shiny::reactive(NULL),
                                     required_args = NULL,
                                     contract_message = NULL) {
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
        # At rest, keep the footer bar present but empty. Removing it would
        # shrink the frame by its height and reflow the whole block upward on
        # every Run; a persistent (quiet) bar holds the frame height constant.
        # Constant height across states is enforced via `min-height` in CSS.
        return(shiny::div(class = "blockr-code-footer blockr-code-footer--rest"))
      }
      perr <- parse_error(ed)
      if (!is.null(perr)) {
        # Syntax error in the pending edit: running it makes no sense, so the
        # Run button is disabled and the footer turns red with the message.
        # Only a parse error is shown here — runtime errors go through the
        # normal evaluation system, not this footer.
        code_block_footer(
          shiny::span(class = "blockr-code-syntax-msg", parse_error_brief(perr),
                      title = perr),
          shiny::actionButton(ns("submit_fn"), "Run",
                              class = "blockr-code-btn blockr-code-btn--run",
                              disabled = TRUE)
        )
      } else if (!is.null(cerr <- contract_error(ed, required_args,
                                                 contract_message))) {
        # Parses fine, but the signature breaks the block's contract (e.g. first
        # arg isn't `data`). It would fail at run time anyway, so disable Run and
        # show the contract message in the bar — same treatment as a syntax error.
        code_block_footer(
          shiny::span(class = "blockr-code-syntax-msg", cerr, title = cerr),
          shiny::actionButton(ns("submit_fn"), "Run",
                              class = "blockr-code-btn blockr-code-btn--run",
                              disabled = TRUE)
        )
      } else {
        code_block_footer(
          "Pending edits",
          # Surface the Mod-Enter shortcut: a muted return glyph on the button,
          # full "Cmd/Ctrl + Enter" on hover. Only on the enabled Run — the
          # disabled error-state buttons stay plain since the shortcut is gated
          # on the code parsing.
          shiny::actionButton(
            ns("submit_fn"),
            shiny::tagList("Run", shiny::span(class = "blockr-code-kbd", "↵")),
            class = "blockr-code-btn blockr-code-btn--run",
            title = "Run (⌘/Ctrl + Enter)"
          )
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


#' The gear icon (same path as `Blockr.icons.gear`), `fill="currentColor"`.
#' @noRd
gear_svg <- function() {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16">',
    '<path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 ',
    '1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82',
    '.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 ',
    '.872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 ',
    '2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31',
    '.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-',
    '.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-',
    '1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 ',
    '2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
  )
}


#' Gear-toggled inline editor section (the authoring surface behind the gear).
#'
#' The standard ecosystem gear button toggles an inline `.blockr-gear-section`
#' holding the editor (and any `top` UI above it — e.g. a template picker).
#' Opening expands it in normal flow and pushes the content below it *down*
#' (not a popover overlay); the gear stays `.blockr-gear-active` (coloured)
#' while open. Styling is self-contained in code-block.css.
#'
#' @param ns Namespace function.
#' @param fn_text Initial function code.
#' @param top Optional UI rendered above the editor inside the section.
#' @param label Editor field label.
#' @return A Shiny tag.
#' @noRd
gear_editor_ui <- function(ns, fn_text, top = NULL, label = "Function code") {
  sec_id <- ns("fn-editor")
  btn_id <- ns("fn-gear-btn")
  shiny::div(
    class = "blockr-gear-editor",
    shiny::div(
      class = "blockr-gear-header",
      shiny::tags$button(
        id = btn_id,
        type = "button",
        class = "blockr-gear-btn",
        title = "Edit function",
        onclick = sprintf(
          "(function(){var s=document.getElementById('%s');var b=document.getElementById('%s');var open=s.style.display!=='none';s.style.display=open?'none':'block';b.classList.toggle('blockr-gear-active',!open);if(!open){%s}})();",
          sec_id, btn_id, code_editor_refresh_js(ns("fn_code"))
        ),
        htmltools::HTML(gear_svg())
      )
    ),
    shiny::div(
      id = sec_id,
      class = "blockr-gear-section",
      style = "display: none;",
      top,
      code_editor_ui(ns, fn_text, label = label)
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
