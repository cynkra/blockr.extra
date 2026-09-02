#' The inputs layer of the code block
#'
#' The code block's script is ordinary R. A top-level assignment whose
#' right-hand side is a *plain value* ([CB_VALUE_CALLS]) becomes a control on
#' the card; every other statement is code. A name starting with a dot is
#' never a control, which is how a line that happens to look like a
#' declaration says it is scaffolding.
#'
#' Both are decidable by reading the line, without evaluating anything and
#' without regard to where in the script it sits. There is deliberately no
#' fenced region, no marker comment and no header: an input is a *kind of
#' line*, not a *place in the script*, so nothing has to be delimited and
#' nothing can be mis-delimited. The editor paints the lines that became
#' controls (see `blockr-code-inputs` in `srcjs/code-block/index.js`), which is
#' what tells the user which lines are special.
#'
#' @name code-block-inputs
#' @keywords internal
NULL


#' Calls whose result counts as a plain value
#'
#' Kept short on purpose. Every addition widens what silently becomes a control,
#' so this is a deliberate list rather than a heuristic. `factor()` is the
#' important one: its levels are the choice list, which is how a select is
#' expressed without any blockr vocabulary.
#'
#' @keywords internal
CB_VALUE_CALLS <- c("c", "factor", "as.Date", "as.POSIXct")

#' Annotation keys accepted after `#|`
#' @noRd
CB_ANN_KEYS <- c("label", "min", "max", "step", "placeholder")

#' Call names accepted as an annotation wrapper (`#| number(min = 1)`)
#' @noRd
CB_ANN_CALLS <- c("number", "text", "select", "flag", "date", "input")

#' The gutter glyph naming each widget
#'
#' Read off the gutter, these say what the whole UI is without a word of prose.
#' Kept to glyphs that render legibly at gutter size in a UI font — the
#' ballot-box and geometric-shape characters some of these would naturally use
#' fall back to an emoji face or a hairline at 11px.
#'
#' @noRd
CB_GLYPHS <- c(select = "\u25be", number = "#", text = "Aa", flag = "\u2713",
               date = "\u25a4")


#' Is this statement an assignment to a bare name?
#'
#' The shape a declaration has to have, whether or not it turns out to be one.
#'
#' @param e A top-level expression from [parse()].
#' @noRd
cb_is_assign_stmt <- function(e) {
  if (!is.call(e) || length(e) != 3L) {
    return(FALSE)
  }
  op <- e[[1L]]
  if (!identical(op, quote(`<-`)) && !identical(op, quote(`=`))) {
    return(FALSE)
  }
  is.name(e[[2L]])
}


#' Is this a name the block keeps to itself?
#'
#' The escape hatch, and it is the one R already has: a leading dot means
#' internal. `ls()` hides those names, so does a file manager, and so does
#' this. It is how a line that would otherwise be read as a knob says it is
#' scaffolding for the knobs around it.
#'
#' @param name A variable name.
#' @noRd
cb_is_private_name <- function(name) {
  !is.na(name) && startsWith(name, ".")
}


#' Is this statement an input declaration?
#'
#' @param e A top-level expression from [parse()].
#' @return `TRUE` for `name <- <plain value>`, the name not starting with a dot.
#' @noRd
cb_is_input_stmt <- function(e) {
  if (!cb_is_assign_stmt(e)) {
    return(FALSE)
  }
  if (cb_is_private_name(as.character(e[[2L]]))) {
    return(FALSE)
  }
  cb_is_value_rhs(e[[3L]])
}


#' Is this right-hand side a plain value?
#'
#' Purely syntactic. Errs toward "not a control": `6L * 2` is arithmetic, not a
#' literal, so it stays code. Being conservative here means a line never becomes
#' a widget by surprise; the reverse mistake only costs an explicit edit.
#'
#' @param rhs The right-hand side expression.
#' @noRd
cb_is_value_rhs <- function(rhs) {
  # A bare literal: 6, "total", TRUE, 2.5. Length-1 only; a longer atomic
  # constant cannot appear in source without a call to `c()`.
  if (is.atomic(rhs) && length(rhs) == 1L) {
    return(TRUE)
  }
  if (!is.call(rhs)) {
    return(FALSE)
  }
  head <- rhs[[1L]]
  # `-1` / `+1` parse as unary calls, not literals.
  if (is.name(head) && as.character(head) %in% c("-", "+") &&
      length(rhs) == 2L && is.numeric(rhs[[2L]])) {
    return(TRUE)
  }
  nm <- cb_call_name(head)
  !is.null(nm) && nm %in% CB_VALUE_CALLS
}


#' The bare name of a call head, following a `::` prefix.
#' @noRd
cb_call_name <- function(head) {
  if (is.name(head)) {
    return(as.character(head))
  }
  if (is.call(head) && identical(head[[1L]], quote(`::`))) {
    return(as.character(head[[3L]]))
  }
  NULL
}


#' Split a script into input declarations and body statements
#'
#' @param text The script.
#' @return A list with `ok`, `error` (parse message or `NULL`), `stmts` (one
#'   record per top-level statement: `expr`, `line`, `input`, `name`) and
#'   `lines` (the raw source lines, for annotation lookup).
#' @noRd
cb_parse <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(ok = TRUE, error = NULL, stmts = list(), lines = character()))
  }
  exprs <- tryCatch(
    parse(text = text, keep.source = TRUE),
    error = function(e) e
  )
  if (inherits(exprs, "error")) {
    return(list(ok = FALSE, error = conditionMessage(exprs), stmts = list(),
                lines = strsplit(text, "\n", fixed = TRUE)[[1L]]))
  }

  refs <- utils::getSrcref(exprs)
  stmts <- lapply(seq_along(exprs), function(i) {
    e <- exprs[[i]]
    is_input <- cb_is_input_stmt(e)
    list(
      expr = e,
      line = if (is.null(refs) || is.null(refs[[i]])) NA_integer_ else
        as.integer(refs[[i]])[1L],
      input = is_input,
      name = if (is_input) as.character(e[[2L]]) else NA_character_
    )
  })

  list(ok = TRUE, error = NULL, stmts = stmts,
       lines = strsplit(text, "\n", fixed = TRUE)[[1L]])
}


#' Read the `#|` annotation off a source line
#'
#' The annotation is the slip road, not the main road: the widget kind already
#' follows from the value, so this only carries what a value cannot say (a
#' numeric range, a nicer label, a placeholder). Accepts both
#' `#| number(min = 1, max = 50)` and the bare `#| min = 1, max = 50`.
#'
#' @param line_text One line of source.
#' @return A named list of evaluated annotation arguments (possibly empty).
#' @noRd
cb_annotation <- function(line_text) {
  if (is.na(line_text) || !grepl("#|", line_text, fixed = TRUE)) {
    return(list())
  }
  m <- regmatches(line_text, regexpr("#\\|.*$", line_text))
  if (!length(m)) {
    return(list())
  }
  body <- trimws(sub("^#\\|", "", m))
  if (!nzchar(body)) {
    return(list())
  }

  e <- tryCatch(parse(text = body)[[1L]], error = function(e) NULL)
  args <- NULL
  if (!is.null(e) && is.call(e) && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% CB_ANN_CALLS) {
    args <- as.list(e)[-1L]
  } else {
    e2 <- tryCatch(parse(text = paste0("list(", body, ")"))[[1L]],
                   error = function(e) NULL)
    if (!is.null(e2)) {
      args <- as.list(e2)[-1L]
    }
  }
  if (is.null(args) || !length(args) || is.null(names(args))) {
    return(list())
  }
  args <- args[names(args) %in% CB_ANN_KEYS]
  if (!length(args)) {
    return(list())
  }
  lapply(args, function(a) tryCatch(eval(a, baseenv()), error = function(e) NULL))
}


#' Derive the widget from a declaration's value
#'
#' The three facts a control needs — kind, choice pool, single or multiple —
#' all come from the *declaration*, never from the live value. A multi-select
#' whose user narrows it to one pick must stay a multi-select, so `multiple` is
#' fixed by the script.
#'
#' Exactly one pick declares a single select; anything else declares a
#' multi-select. **Zero is deliberately on the multi side**, so a script can
#' offer a set of choices with nothing chosen yet
#' (`factor(character(0), levels = lv)`). Reading it as a single select instead
#' would leave no way to write "pick any number of these, starting from none" —
#' the declaration's length is the only signal available, and a one-element
#' default is not always wanted.
#'
#' @param v The evaluated right-hand side.
#' @return A list with `kind`, and for selects `choices` and `multiple`; `NULL`
#'   when the value maps to no widget.
#' @noRd
cb_widget_for <- function(v) {
  if (is.factor(v)) {
    return(list(kind = "select", choices = levels(v),
                multiple = length(v) != 1L))
  }
  if (inherits(v, "Date")) {
    return(list(kind = "date"))
  }
  if (is.logical(v) && length(v) == 1L && !is.na(v)) {
    return(list(kind = "flag"))
  }
  if (is.numeric(v) && length(v) == 1L) {
    return(list(kind = "number"))
  }
  if (is.character(v) && length(v) == 1L) {
    return(list(kind = "text"))
  }
  # A bare vector is the value, so the pool is exactly that vector. Honest, if
  # not very useful — widen it with a factor.
  if ((is.character(v) || is.numeric(v)) && length(v) > 1L) {
    return(list(kind = "select", choices = as.character(v), multiple = TRUE))
  }
  NULL
}


#' Build the input specs for a script
#'
#' Declarations are evaluated in the block's own evaluation environment (so a
#' declaration can reach exactly what the body can reach, no more). Preceding
#' non-input assignments are bound lazily, so a helper line like
#' `lv <- unique(data$site)` is available to a later `factor(x, lv)` without the
#' pipeline above it ever being run.
#'
#' A statement that is not an assignment cannot be bound lazily, so the ones
#' *above the last declaration* are simply run: a preamble that works the
#' grouping column out with an `if` has to have happened before the
#' declarations under it are evaluated. Nothing below the last declaration is
#' touched, which is where the pipeline lives.
#'
#' @param parsed The result of [cb_parse()].
#' @param data The upstream data frame (may be `NULL` before it arrives).
#' @return A list of spec records.
#' @noRd
cb_specs <- function(parsed, data = NULL) {
  if (!parsed$ok || !length(parsed$stmts)) {
    return(list())
  }

  env <- blockr.core::eval_env(list(data = data))
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  last_decl <- if (any(is_input)) max(which(is_input)) else 0L
  # A declaration the script uses as scaffolding is not a knob: it is bound
  # like any other helper line and no control is offered. See cb_demoted().
  demoted <- cb_demoted(parsed)
  specs <- list()

  for (i in seq_along(parsed$stmts)) {
    st <- parsed$stmts[[i]]
    if (!st$input || demoted[[i]]) {
      # Bind other assignments lazily: only forced if a later declaration
      # actually reads them.
      if (cb_is_assign_stmt(st$expr)) {
        cb_delay(env, as.character(st$expr[[2L]]), st$expr[[3L]])
      } else if (i < last_decl) {
        # Not an assignment, so there is nothing to defer. A declaration below
        # it may depend on what it does, and it fails on its own account when
        # the block runs, so a failure here is not reported twice.
        try(eval(st$expr, env), silent = TRUE)
      }
      next
    }

    value <- tryCatch(eval(st$expr[[3L]], env), error = function(e) e)
    if (inherits(value, "error")) {
      specs[[length(specs) + 1L]] <- list(
        name = st$name, line = st$line, kind = NA_character_,
        error = conditionMessage(value)
      )
      next
    }
    # Make it visible to later declarations too.
    assign(st$name, value, envir = env)

    w <- cb_widget_for(value)
    if (!is.null(w) && identical(w$kind, "select") && !length(w$choices)) {
      # An empty factor is almost always a mistyped column name; an empty
      # dropdown would hide that.
      specs[[length(specs) + 1L]] <- list(
        name = st$name, line = st$line, kind = NA_character_,
        error = "no choices - is the column name right?"
      )
      next
    }
    if (is.null(w)) {
      specs[[length(specs) + 1L]] <- list(
        name = st$name, line = st$line, kind = NA_character_,
        error = paste0("no control for a value of class ",
                       paste(class(value), collapse = "/"))
      )
      next
    }

    ann <- cb_annotation(
      if (is.na(st$line) || st$line > length(parsed$lines)) NA_character_
      else parsed$lines[[st$line]]
    )

    specs[[length(specs) + 1L]] <- c(
      list(
        name = st$name,
        line = st$line,
        kind = w$kind,
        choices = w$choices,
        multiple = isTRUE(w$multiple),
        default = if (is.factor(value)) as.character(value) else value,
        error = NULL
      ),
      ann[setdiff(names(ann), c("name", "line", "kind", "choices", "multiple",
                                "default", "error"))]
    )
  }

  specs
}


#' Bind a name to an expression that is only evaluated if something reads it
#'
#' A declaration may legitimately depend on a helper line above it
#' (`lv <- unique(data$site)` feeding `factor("Basel", lv)`), but the statements
#' in between must not run just to build that scope — one of them is usually the
#' block's whole pipeline. An active binding evaluates on first read and caches.
#'
#' @noRd
cb_delay <- function(env, name, expr) {
  force(expr)
  force(env)
  cached <- NULL
  forced <- FALSE
  # A later statement may re-assign a name an earlier declaration already
  # bound; script order wins, and `makeActiveBinding()` refuses to shadow a
  # regular binding. (`exists()` would force an active binding, so check the
  # names directly.)
  #
  # `x <- f(x)` is ordinary R and reads the OLD `x`, so the old binding is
  # carried into a child environment the new one evaluates in. Without that,
  # the binding reads itself and the declaration below it dies with "infinite
  # recursion" instead of seeing the narrowed value.
  where <- env
  if (name %in% ls(env, all.names = TRUE)) {
    where <- new.env(parent = env)
    if (bindingIsActive(name, env)) {
      makeActiveBinding(name, activeBindingFunction(name, env), where)
    } else {
      assign(name, get(name, envir = env, inherits = FALSE), envir = where)
    }
    rm(list = name, envir = env)
  }
  # The binding has to accept a write too: assigning over an active binding
  # CALLS it with the new value, so a nullary function dies with "unused
  # argument" the moment a later declaration of the same name is evaluated
  # into this environment.
  makeActiveBinding(
    name,
    function(v) {
      if (!missing(v)) {
        cached <<- v
        forced <<- TRUE
        return(invisible(v))
      }
      if (!forced) {
        cached <<- eval(expr, where)
        forced <<- TRUE
      }
      cached
    },
    env
  )
}


#' Coerce a value coming back from a widget to the declared type
#'
#' Shiny inputs arrive as strings from selects and as lists from JS arrays; the
#' declaration already knows the type, so it drives the coercion. Numeric `NA`
#' notably ships as the *string* `"NA"`.
#'
#' @param value The raw input value.
#' @param spec The spec record it belongs to.
#' @noRd
cb_coerce <- function(value, spec) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.list(value)) {
    value <- unlist(value, use.names = FALSE)
  }
  if (is.null(value) || !length(value)) {
    return(if (identical(spec$kind, "select") && isTRUE(spec$multiple))
      character() else NULL)
  }
  switch(
    spec$kind,
    number = {
      v <- suppressWarnings(as.numeric(value))
      if (all(is.na(v))) NULL else v
    },
    flag = as.logical(value),
    date = as.Date(value),
    text = as.character(value),
    select = {
      out <- as.character(value)
      # A numeric-valued select round-trips through the DOM as text.
      if (is.numeric(spec$default)) {
        num <- suppressWarnings(as.numeric(out))
        if (!any(is.na(num))) out <- num
      }
      out
    },
    value
  )
}


#' Render one control
#'
#' @param spec A spec record.
#' @param ns Namespace function.
#' @param value The value to show (falls back to the declaration's).
#' @noRd
cb_input_ui <- function(spec, ns, value = NULL) {
  input_id <- ns(paste0("cb_", spec$name))
  label <- spec$label
  if (is.null(label)) {
    label <- gsub("[._]", " ", spec$name)
    label <- paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))
  }
  if (is.null(value)) {
    value <- spec$default
  }

  inner <- switch(
    spec$kind,
    select = {
      # Keep only picks the current data still offers, so a changed upstream
      # narrows the selection instead of sending a value that is no longer a
      # choice.
      sel <- intersect(as.character(value), as.character(spec$choices))
      if (!length(sel) && !spec$multiple) {
        sel <- as.character(spec$choices)[1L]
      }
      fb_select_input(
        input_id = input_id,
        label = label,
        choices = spec$choices,
        selected = sel,
        multiple = spec$multiple
      )
    },
    # An absent annotation must reach numericInput() as NA, not NULL: it tests
    # its limits with `if (!is.na(min))`, and NULL makes that a length-zero
    # condition.
    number = shiny::numericInput(
      inputId = input_id, label = label, value = value,
      min = cb_or_na(spec$min), max = cb_or_na(spec$max),
      step = cb_or_na(spec$step)
    ),
    text = shiny::textInput(
      inputId = input_id, label = label, value = value,
      placeholder = spec$placeholder
    ),
    flag = shiny::checkboxInput(
      inputId = input_id, label = label, value = isTRUE(value)
    ),
    date = shiny::dateInput(
      inputId = input_id, label = label, value = value
    ),
    NULL
  )
  if (is.null(inner)) {
    return(NULL)
  }
  shiny::div(class = "block-input-wrapper", inner)
}


#' The params grid holding every control
#' @noRd
cb_params_ui <- function(specs, ns, values = list()) {
  ok <- Filter(function(s) is.null(s$error) && !is.na(s$kind), specs)
  if (!length(ok)) {
    return(NULL)
  }
  fields <- lapply(ok, function(s) cb_input_ui(s, ns, values[[s$name]]))
  fields <- Filter(Negate(is.null), fields)
  if (!length(fields)) {
    return(NULL)
  }
  shiny::div(
    class = "fb-params-grid",
    style = fb_grid_cols(length(fields)),
    fields
  )
}


#' The per-line marks the editor paints
#'
#' One record per input line: the 1-based line number and a short glyph naming
#' the widget. Pushed to the client as `blockr-code-inputs`.
#'
#' @noRd
cb_editor_marks <- function(specs) {
  marks <- lapply(specs, function(s) {
    if (is.na(s$line)) {
      return(NULL)
    }
    kind <- if (is.null(s$error) && !is.na(s$kind)) s$kind else NA_character_
    list(
      line = s$line,
      kind = if (is.na(kind)) "error" else kind,
      glyph = if (is.na(kind)) "!" else unname(CB_GLYPHS[[kind]]),
      title = if (!is.null(s$error)) {
        paste0(s$name, ": ", s$error)
      } else {
        cb_mark_title(s)
      }
    )
  })
  unname(Filter(Negate(is.null), marks))
}

#' @noRd
cb_mark_title <- function(s) {
  switch(
    s$kind,
    select = sprintf(
      "%s-select · %d choice%s",
      if (isTRUE(s$multiple)) "multi" else "single",
      length(s$choices), if (length(s$choices) == 1L) "" else "s"
    ),
    number = "number",
    text = "text",
    flag = "checkbox",
    date = "date",
    s$kind
  )
}


#' Classify input lines without evaluating anything
#'
#' The editor's band and gutter glyphs are pushed on every (debounced)
#' keystroke, so they must not evaluate declarations against the data — the
#' point is that you type a line and watch the glyph appear. The widget kind
#' follows from the *shape* of the right-hand side, which is enough for a
#' glyph; the real specs (choices, multiplicity, defaults) still come from
#' [cb_specs()] on the committed script.
#'
#' Re-assignment is syntactic too, so a declaration the script overwrites
#' further down gets no band either: what the editor paints and what turns into
#' a control are the same set of lines.
#'
#' @param text The script (usually the live editor text).
#' @return The same mark records [cb_editor_marks()] produces.
#' @noRd
cb_syntactic_marks <- function(text) {
  parsed <- cb_parse(text)
  if (!parsed$ok) {
    return(list())
  }
  title <- c(select = "select", number = "number", text = "text",
             flag = "checkbox", date = "date")
  demoted <- cb_demoted(parsed)
  marks <- lapply(seq_along(parsed$stmts), function(i) {
    st <- parsed$stmts[[i]]
    if (!st$input || is.na(st$line) || demoted[[i]]) {
      return(NULL)
    }
    kind <- cb_kind_syntactic(st$expr[[3L]])
    if (is.null(kind)) {
      return(NULL)
    }
    list(line = st$line, kind = kind, glyph = unname(CB_GLYPHS[[kind]]),
         title = unname(title[[kind]]))
  })
  unname(Filter(Negate(is.null), marks))
}


#' The widget kind implied by the shape of a right-hand side
#' @noRd
cb_kind_syntactic <- function(rhs) {
  if (is.call(rhs)) {
    nm <- cb_call_name(rhs[[1L]])
    if (!is.null(nm)) {
      if (nm %in% c("-", "+")) {
        return("number")
      }
      return(switch(nm,
        factor = "select",
        c = "select",
        as.Date = "date",
        as.POSIXct = "date",
        NULL
      ))
    }
    return(NULL)
  }
  if (is.logical(rhs)) {
    return("flag")
  }
  if (is.numeric(rhs)) {
    return("number")
  }
  if (is.character(rhs)) {
    return("text")
  }
  NULL
}


#' `NA` for an absent annotation value
#' @noRd
cb_or_na <- function(x) {
  if (is.null(x) || !length(x)) NA else x
}
