#' Render a block result, dispatching on the result's class
#'
#' `block_output()` dispatches on the BLOCK, which is the right hinge for a
#' block that always draws the same kind of thing. A function or code block
#' draws whatever its code returned, so the thing worth dispatching on is the
#' RESULT: a data frame, a gt table, a ggplot, a composer table.
#'
#' The default method is `render_dynamic_output()`, the type switch this
#' package has always used, so adding this generic changes nothing on its
#' own. It exists so another package can teach a function block to draw its
#' own object without that object's renderer having to live here -- and,
#' unlike an `htmltools::as.tags()` method, a method here receives the block's
#' module `session`. That is what a renderer needs to namespace an element id,
#' register a `downloadHandler` on `session$output`, or observe a click on
#' `session$input`.
#'
#' The known case is composer: `blockr.sandbox` registers a `composed_table`
#' method that draws the table through blockr.viz's renderer rather than
#' composer's gt.
#'
#' @section Called on every re-evaluation:
#' blockr.core re-runs `block_output()` whenever the block's result changes
#' (`output_render_observer()`), so a method that creates observers must guard
#' them (once per session, keyed on the namespace). Assigning to
#' `session$output` is idempotent and needs no guard.
#'
#' @param result The block's evaluated result.
#' @param block The block object.
#' @param session The block's Shiny module session.
#' @param ... Passed to methods.
#' @return A shiny render function, as [blockr.core::block_output()] requires.
#' @export
block_result_output <- function(result, block, session, ...) {
  UseMethod("block_result_output")
}

#' @rdname block_result_output
#' @export
block_result_output.default <- function(result, block, session, ...) {
  render_dynamic_output(result, block, session)
}

# A composer table is counts, and a drill on it has to find the subjects behind
# a count. The frame this block received is where they are, after every step
# above the block (a flag filter, a group filter), so it rides along as
# `source_data`, the attribute the composer drill resolves a click against
# (blockr.sandbox `composer_drill_claims()`, blockr.viz `drill_source()`).
# A script that stamped its own, narrower frame keeps it. The block's code is
# untouched: this happens to the result, not the expression.
stamp_source_data <- function(res, env) {
  if (!inherits(res, "composed")) {
    return(res)
  }
  if (!is.null(attr(res, "source_data", exact = TRUE)) ||
        !is.null(res[["source_data"]])) {
    return(res)
  }
  data <- if (is.environment(env)) {
    get0("data", envir = env, inherits = FALSE)
  } else {
    env[["data"]]
  }
  if (is.data.frame(data)) {
    attr(res, "source_data") <- data
  }
  res
}
