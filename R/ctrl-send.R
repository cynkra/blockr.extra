#' Cross-block external control
#'
#' A pair of helpers that let one block set the externally controllable
#' arguments (see the `external_ctrl` argument to [blockr.core::new_block()])
#' of another block on the same board, through the board's regular update
#' channel. Typical use: a drill-down chart feeds a [new_function_block()]
#' whose function calls `ctrl_send()` to push the clicked patient into a
#' patient-profile block -- no data link between sender and target, so lazy
#' evaluation never drags the sender's upstream into the target's closure.
#'
#' `install_ctrl_send()` wires the channel up. It must be called once per
#' session by any board component that holds the board's `update` reactive:
#' a board callback, a plugin, or a dock extension server. It stashes a send
#' closure in `session$userData`, where `ctrl_send()` -- called from block
#' code, e.g. inside a function block's `fn` -- picks it up.
#'
#' Updates pushed this way go through the board's full validation:
#' arguments that are not externally controllable on the target block are
#' rejected with a user-facing notification (see
#' `blockr.core:::validate_board_update`).
#'
#' @section Semantics under lazy evaluation:
#' `ctrl_send()` writes state; it never reads another block's result, so it
#' adds nothing to any evaluation closure. A function block calling it is
#' re-evaluated (and thus sends) only while it is on screen, which is
#' exactly when its upstream chart can be clicked. Repeated sends of an
#' unchanged value are no-ops (`apply_block_mod_delta()` compares with
#' `identical()`).
#'
#' @param target Character(1). Block id of the target block on the board.
#' @param ... Named values to set, one per externally controllable argument
#'   of the target block, e.g. `subject = "01-701-1015"`.
#' @param session Shiny session (defaults to the current reactive domain).
#'
#' @return `ctrl_send()` returns `TRUE` (invisibly) when the payload was
#'   handed to the update channel, `FALSE` when no channel is installed
#'   (a warning is emitted once per session). `ctrl_clear()` returns `TRUE`
#'   (invisibly) when the caller owned the target's last claim and the reset
#'   was sent, `FALSE` otherwise (not the owner, or no channel installed).
#'   `install_ctrl_send()` returns the send closure, invisibly.
#'
#' @examples
#' # Inside a function block fn (always qualify: fns are deparsed):
#' # function(data) {
#' #   ids <- unique(data$USUBJID)
#' #   if (length(ids) == 1L) {
#' #     blockr.extra::ctrl_send("profile", subject = ids)
#' #   }
#' #   data
#' # }
#'
#' @export
ctrl_send <- function(target, ..., session = shiny::getDefaultReactiveDomain()) {

  stopifnot(
    is.character(target), length(target) == 1L, nzchar(target),
    !is.null(session)
  )

  args <- list(...)

  if (length(args) == 0L || is.null(names(args)) || !all(nzchar(names(args)))) {
    stop("`ctrl_send()` expects named values, one per controllable argument.")
  }

  send <- session$userData$blockr_ctrl_send

  if (!is.function(send)) {
    if (!isTRUE(session$userData$blockr_ctrl_send_warned)) {
      session$userData$blockr_ctrl_send_warned <- TRUE
      warning(
        "ctrl_send(): no control channel installed for this board ",
        "(missing `install_ctrl_send()` in a board callback or extension); ",
        "ignoring.",
        call. = FALSE
      )
    }
    return(invisible(FALSE))
  }

  send(target, args, author = ctrl_author(session))

  invisible(TRUE)
}

#' @section Clearing (`ctrl_clear()`):
#' The undo side of `ctrl_send()`, with ownership semantics: it resets the
#' target's controllable arguments **only if the last `ctrl_send()` to that
#' target came from the same block**. This is what lets a data-driven sender
#' propagate an un-drill (its input reverts to the no-claim shape, so it
#' calls `ctrl_clear()`) without ever clobbering state it does not own: a
#' sender that merely re-evaluates undrilled -- at startup, on a board
#' restore, or while a *different* sender's claim (or a manual edit riding
#' on one) is active on the target -- finds it owns nothing and no-ops.
#' Named values passed via `...` are what "cleared" means for the target
#' (e.g. `state = list(columns = list())` empties a value filter).
#'
#' @rdname ctrl_send
#' @export
ctrl_clear <- function(target, ..., session = shiny::getDefaultReactiveDomain()) {

  stopifnot(
    is.character(target), length(target) == 1L, nzchar(target),
    !is.null(session)
  )

  args <- list(...)

  if (length(args) == 0L || is.null(names(args)) || !all(nzchar(names(args)))) {
    stop("`ctrl_clear()` expects named reset values, one per controllable ",
         "argument.")
  }

  clear <- session$userData$blockr_ctrl_clear

  if (!is.function(clear)) {
    return(invisible(FALSE))
  }

  invisible(clear(target, args, author = ctrl_author(session)))
}

# The calling block's identity: the module namespace of the session the
# helper is invoked from (ctrl_send()/ctrl_clear() run inside the block's
# own server / expr evaluation). Used only to scope clears to their author.
ctrl_author <- function(session) {
  ns <- tryCatch(session$ns(""), error = function(e) NULL)
  if (is.null(ns) || !nzchar(ns)) "(root)" else ns
}

#' @param update The board update reactive, as handed to board callbacks,
#'   plugins and dock extension servers.
#'
#' @rdname ctrl_send
#' @export
install_ctrl_send <- function(update, session = shiny::getDefaultReactiveDomain()) {

  stopifnot(is.function(update), !is.null(session))

  # Last-author registry, one entry per target block id. `ctrl_send()`
  # claims authorship; `ctrl_clear()` only acts when the caller still holds
  # it (and releases it when it does).
  authors <- new.env(parent = emptyenv())

  send <- function(target, args, author = NULL) {
    if (!is.null(author)) assign(target, author, envir = authors)
    update(
      list(blocks = list(mod = stats::setNames(list(args), target)))
    )
  }

  clear <- function(target, args, author = NULL) {
    owner <- if (exists(target, envir = authors)) get(target, envir = authors)
    if (is.null(author) || !identical(owner, author)) {
      return(FALSE)
    }
    rm(list = target, envir = authors)
    update(
      list(blocks = list(mod = stats::setNames(list(args), target)))
    )
    TRUE
  }

  session$userData$blockr_ctrl_send <- send
  session$userData$blockr_ctrl_clear <- clear

  invisible(send)
}
