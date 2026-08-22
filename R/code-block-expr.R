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
  # `x$name` / `x@name`: the right side is a literal name, not a variable.
  if (length(e) == 3L &&
      (identical(e[[1L]], quote(`$`)) || identical(e[[1L]], quote(`@`)))) {
    e[[2L]] <- cb_subst(e[[2L]], subs)
    return(e)
  }
  # Index 1 is the call head, which is never an input.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      e[[i]] <- cb_subst(e[[i]], subs)
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
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      e[[i]] <- cb_fold(e[[i]])
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
  shadowed <- cb_shadowed(parsed)
  usable <- vapply(
    specs,
    function(s) is.null(s$error) && !is.na(s$kind) && !s$name %in% shadowed,
    logical(1L)
  )
  specs <- specs[usable]

  body <- Filter(
    function(st) !st$input || st$name %in% shadowed,
    parsed$stmts
  )
  body <- lapply(body, `[[`, "expr")
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
