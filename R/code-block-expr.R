#' Compiling a code-block script into a block expression
#'
#' The block does not wrap the user's code in a function and call it. It
#' substitutes the current input values into the body as literals and returns
#' that. Two consequences follow, and they are the point of the block:
#'
#' * The expression contains no assignment, so `blockr.code`'s `has_assignment()`
#'   is false and the exported script is emitted bare — no `local({...})` wrapper.
#' * The exported code is the code an analyst would have written, with the knob
#'   positions written in: `dplyr::filter(block_1, Species == "setosa")`.
#'
#' @name code-block-expr
#' @keywords internal
NULL


#' Marker left behind by a folded-away `if` with no `else`.
#' @noRd
CB_DROP <- quote(`__cb_drop__`)


#' Is this the empty symbol (a missing argument, as in `x[, 1]`)?
#' @noRd
cb_is_empty_sym <- function(x) {
  is.name(x) && !nzchar(as.character(x))
}


#' Turn a value into an AST node that deparses idiomatically
#'
#' Most values can be inlined as themselves — a character vector deparses to
#' `c("a", "b")`, an integer to `6L`. Dates and datetimes cannot: they would
#' deparse to `structure(20318, class = "Date")`, so they are re-emitted as the
#' constructor call instead.
#'
#' @param v A value.
#' @noRd
cb_literal <- function(v) {
  if (inherits(v, "Date")) {
    return(bquote(as.Date(.(format(v, "%Y-%m-%d")))))
  }
  if (inherits(v, "POSIXct")) {
    tz <- attr(v, "tzone")
    txt <- format(v, "%Y-%m-%d %H:%M:%S")
    if (is.null(tz) || !nzchar(tz)) {
      return(bquote(as.POSIXct(.(txt))))
    }
    return(bquote(as.POSIXct(.(txt), tz = .(tz))))
  }
  if (is.factor(v)) {
    return(as.character(v))
  }
  v
}


#' Substitute symbols throughout an expression
#'
#' Only value positions are touched: never a call head, never the right side of
#' `$` or `@`, never an argument name. Those are the positions where a symbol is
#' not a variable reference, and substituting them is how naive source rewriting
#' produces broken code.
#'
#' Scope is respected too. A `function()` formal rebinds the name for that
#' body, so the substitution stops at the boundary; a `for` variable rebinds it
#' in the same environment, so the declaration is demoted to code instead (see
#' [cb_shadowed()]).
#'
#' @param e An expression.
#' @param subs A named list of replacement nodes.
#' @noRd
cb_subst <- function(e, subs) {
  if (is.name(e)) {
    nm <- as.character(e)
    if (nzchar(nm) && nm %in% names(subs)) {
      return(subs[[nm]])
    }
    return(e)
  }
  if (!is.call(e)) {
    return(e)
  }
  # `function(x) ...` opens a scope: a formal binds the name for the body, so
  # the control of the same name is not what that body means. Substituting it
  # would rewrite `function(data) nrow(data)` into a reference to the upstream
  # block and quietly return the wrong number. A formal shadows for the
  # defaults too, which R evaluates in the function's own frame.
  if (identical(e[[1L]], quote(`function`)) && length(e) >= 3L) {
    inner <- subs[setdiff(names(subs), names(e[[2L]]))]
    fmls <- e[[2L]]
    for (i in seq_along(fmls)) {
      if (!cb_is_empty_sym(fmls[[i]])) {
        fmls[[i]] <- cb_subst(fmls[[i]], inner)
      }
    }
    e[[2L]] <- fmls
    e[[3L]] <- cb_subst(e[[3L]], inner)
    # The srcref would otherwise deparse the pre-substitution source.
    if (length(e) >= 4L) {
      e[[4L]] <- NULL
    }
    return(e)
  }
  # `x <- v`: the left side is the name being bound, not a reference to it. A
  # demoted declaration keeps its line in the body, and if a later declaration
  # of the same name gave it a control, substituting here would rewrite the
  # line into `c("a", "b") <- c("a", "b")`. An indexed target (`x[i] <- v`) is
  # a call, and the index inside it IS a value reference, so only a bare name
  # is skipped.
  if (length(e) == 3L && is.name(e[[2L]]) &&
      (identical(e[[1L]], quote(`<-`)) || identical(e[[1L]], quote(`=`)) ||
         identical(e[[1L]], quote(`<<-`)))) {
    e[[3L]] <- cb_subst(e[[3L]], subs)
    return(e)
  }
  # `x$name` / `x@name`: the right side is a literal name, not a variable.
  if (length(e) == 3L &&
      (identical(e[[1L]], quote(`$`)) || identical(e[[1L]], quote(`@`)))) {
    e[[2L]] <- cb_subst(e[[2L]], subs)
    return(e)
  }
  # Index 1 is the call head, which is never an input.
  # A substitution that comes back NULL must NOT be assigned into the call:
  # `e[[i]] <- NULL` DELETES the element and shortens `e`, while the loop
  # bounds were fixed before it started. A NULL argument in last position then
  # vanishes from the emitted code, and one anywhere else walks the loop off
  # the end ("subscript out of bounds"), killing the block. Intentional
  # removal has its own sentinel (CB_DROP), so NULL here only ever means "this
  # argument is NULL", which is already what the call says.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      sub <- cb_subst(e[[i]], subs)
      if (is.null(sub)) {
        next
      }
      e[[i]] <- sub
    }
  }
  e
}


#' Fold `if` on a condition that substitution turned into a literal
#'
#' A checkbox is the one input that leaves branching behind: without this,
#' every flag would put `if (TRUE) ...` in the exported script. The condition is
#' known at export time, so the untaken branch is simply dropped. This is the
#' one place the emitted code is not a literal substitution of the source.
#'
#' @param e An expression.
#' @noRd
cb_fold <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  # Same trap as cb_subst(): assigning NULL into a call deletes the element.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      folded <- cb_fold(e[[i]])
      if (is.null(folded)) {
        next
      }
      e[[i]] <- folded
    }
  }

  if (identical(e[[1L]], quote(`if`))) {
    cond <- e[[2L]]
    if (is.logical(cond) && length(cond) == 1L && !is.na(cond)) {
      if (cond) {
        return(e[[3L]])
      }
      if (length(e) >= 4L) {
        return(e[[4L]])
      }
      return(CB_DROP)
    }
  }

  cb_prune(e)
}


#' Remove folded-away operands from the shapes they can appear in
#'
#' `p + if (FALSE) geom_smooth()` should emit `p`, not `p + NULL`; a dropped
#' statement inside `{}` should vanish rather than leave a blank.
#'
#' @param e A call whose arguments have already been folded.
#' @noRd
cb_prune <- function(e) {
  is_drop <- function(x) identical(x, CB_DROP)

  if (identical(e[[1L]], quote(`{`)) && length(e) > 1L) {
    keep <- !vapply(as.list(e)[-1L], is_drop, logical(1L))
    return(as.call(c(list(quote(`{`)), as.list(e)[-1L][keep])))
  }
  # Binary operators where an absent operand means "leave the other one".
  if (length(e) == 3L && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% c("+", "|>", "%>%")) {
    if (is_drop(e[[3L]])) {
      return(e[[2L]])
    }
    if (is_drop(e[[2L]])) {
      return(e[[3L]])
    }
  }
  # Anywhere else a dropped branch really is NULL.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (!cb_is_empty_sym(e[[i]]) && is_drop(e[[i]])) {
        e[[i]] <- NULL
      }
    }
  }
  e
}


#' Names assigned anywhere in a set of statements
#'
#' An input whose name the body also assigns is not an input: the script means
#' what plain R would mean, so the declaration stays code and no control is
#' offered. Reported to the user rather than silently resolved.
#'
#' @param exprs A list of expressions.
#' @noRd
cb_assigned_names <- function(exprs) {
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (length(e) == 3L && is.name(e[[2L]]) &&
        (identical(e[[1L]], quote(`<-`)) || identical(e[[1L]], quote(`=`)) ||
           identical(e[[1L]], quote(`<<-`)))) {
      found <<- c(found, as.character(e[[2L]]))
    }
    # A loop variable is an assignment in the same environment: after
    # `for (n in 1:2)` plain R leaves `n` at 2, whatever it was before. So a
    # declaration the body loops over is demoted to code like any other
    # re-assignment, rather than being substituted into the loop head.
    if (identical(e[[1L]], quote(`for`)) && length(e) == 4L &&
          is.name(e[[2L]])) {
      found <<- c(found, as.character(e[[2L]]))
    }
    if (length(e) > 1L) {
      for (i in 2L:length(e)) {
        if (!cb_is_empty_sym(e[[i]])) {
          walk(e[[i]])
        }
      }
    }
    invisible(NULL)
  }
  for (e in exprs) walk(e)
  unique(found)
}


#' Which declarations are demoted back to code by a body assignment?
#'
#' @param parsed The result of [cb_parse()].
#' @noRd
cb_shadowed <- function(parsed) {
  if (!parsed$ok || !length(parsed$stmts)) {
    return(character())
  }
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  if (!any(is_input)) {
    return(character())
  }
  body <- lapply(parsed$stmts[!is_input], `[[`, "expr")
  intersect(
    vapply(parsed$stmts[is_input], `[[`, character(1L), "name"),
    cb_assigned_names(body)
  )
}


#' Which declarations are not controls after all?
#'
#' A declaration is a control unless the script itself uses the line as
#' scaffolding, which shows up in three shapes:
#'
#' * the name is assigned again by a body statement (the knob's value would be
#'   thrown away),
#' * the name is declared a second time further down (only the last declaration
#'   can be the control),
#' * another declaration reads the name (`lv <- unique(data$site)` feeding
#'   `site <- factor("Basel", lv)` is a choice pool for the knob below it, not a
#'   knob of its own).
#'
#' All three are decided by reading the script, without evaluating anything.
#'
#' @param parsed The result of [cb_parse()].
#' @return A logical vector over `parsed$stmts`.
#' @noRd
cb_demoted <- function(parsed) {
  n <- length(parsed$stmts)
  if (!parsed$ok || !n) {
    return(logical(n))
  }
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  nms <- vapply(parsed$stmts, `[[`, character(1L), "name")

  body_assigned <- cb_assigned_names(lapply(parsed$stmts[!is_input], `[[`, "expr"))

  reason <- vapply(seq_len(n), function(i) {
    if (!is_input[[i]]) {
      return("")
    }
    nm <- nms[[i]]
    if (nm %in% body_assigned) {
      return("assigned")
    }
    later <- is_input & seq_len(n) > i
    if (any(later & !is.na(nms) & nms == nm)) {
      return("redeclared")
    }
    others <- which(is_input & seq_len(n) != i)
    read_below <- any(vapply(others, function(j) {
      nm %in% all.names(parsed$stmts[[j]]$expr[[3L]])
    }, logical(1L)))
    if (read_below) "helper" else ""
  }, character(1L))

  out <- nzchar(reason)
  attr(out, "reason") <- reason
  attr(out, "name") <- nms
  out
}


#' Compile the script into the block's expression
#'
#' @param parsed The result of [cb_parse()].
#' @param specs Input specs from [cb_specs()].
#' @param values Named list of current input values.
#' @param data_name Name of the block's data slot.
#' @return A quoted expression carrying the `.(data)` slot, or `NULL` when the
#'   script has no body.
#' @noRd
cb_expr <- function(parsed, specs, values = list(), data_name = "data") {
  if (!parsed$ok) {
    return(NULL)
  }
  demoted <- cb_demoted(parsed)
  usable <- vapply(
    specs,
    function(s) is.null(s$error) && !is.na(s$kind),
    logical(1L)
  )
  specs <- specs[usable]

  # A declaration stays where it was written, as code, unless a usable control
  # replaced it. That covers the demoted lines and, just as importantly, a
  # declaration whose value could not be read: dropping the line while nothing
  # substitutes the name leaves the expression referring to a symbol that does
  # not exist.
  live <- vapply(specs, `[[`, character(1L), "name")
  keep <- vapply(seq_along(parsed$stmts), function(i) {
    st <- parsed$stmts[[i]]
    !st$input || demoted[[i]] || !st$name %in% live
  }, logical(1L))
  body <- lapply(parsed$stmts[keep], `[[`, "expr")
  if (!length(body)) {
    return(NULL)
  }

  subs <- list()
  subs[[data_name]] <- call(".", as.name(data_name))
  for (s in specs) {
    # A restored board hands back whatever survived JSON, so the declaration —
    # which knows the type — drives the coercion before the value is inlined.
    v <- cb_coerce(values[[s$name]], s)
    if (is.null(v) || !length(v)) {
      v <- s$default
    }
    subs[[s$name]] <- cb_literal(v)
  }

  body <- lapply(body, function(e) cb_fold(cb_subst(e, subs)))
  body <- Filter(function(e) !identical(e, CB_DROP), body)
  if (!length(body)) {
    return(NULL)
  }
  if (length(body) == 1L) {
    return(body[[1L]])
  }
  as.call(c(list(quote(`{`)), body))
}
