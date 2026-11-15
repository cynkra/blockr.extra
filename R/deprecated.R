# The code block and the function blocks are superseded by the script block
# in blockr.script. They stay in this package so that saved boards keep
# loading, and they say so in three places: a line on the block above its
# result, a warning in the R log once per session, and "(deprecated)" in the
# block registry (R/registry.R).
#
# Line and warning come from the block's UI (block_ui methods), not from the
# constructors: the block list builds every registered block once at start,
# and a warning from there would name blocks no board uses.

extra_deprecation_text <- paste(
  "Deprecated: use the Script block from blockr.script.",
  "This block stays so that saved boards keep working."
)

#' The line a deprecated block shows above its result
#' @noRd
extra_deprecation_notice <- function() {
  shiny::div(
    class = "blockr-extra-deprecated",
    role = "note",
    style = paste(
      "font-size: var(--blockr-font-size-xs, 0.75rem);",
      "color: var(--blockr-color-text-muted, #6b7280);",
      "margin: 0 0 8px;"
    ),
    extra_deprecation_text
  )
}

# Which constructors have warned in this R session.
extra_deprecation_warned <- new.env(parent = emptyenv())

#' Warn once per session that a constructor is deprecated
#'
#' Goes to the R log, not to the user: a board that draws ten code blocks
#' writes one line, not ten.
#'
#' @param ctor The constructor's name.
#' @noRd
warn_extra_deprecated <- function(ctor) {
  if (isTRUE(extra_deprecation_warned[[ctor]])) {
    return(invisible(NULL))
  }
  assign(ctor, TRUE, envir = extra_deprecation_warned)
  blockr.core::log_warn(
    ctor, "() is deprecated. Use blockr.script::new_script_block() instead.",
    use_glue = FALSE
  )
  invisible(NULL)
}

# What the assistant reads about a deprecated block (R/registry.R).
extra_deprecation_guidance <- paste(
  "DEPRECATED: do not add this block to a board. Add the Script block",
  "(new_script_block from blockr.script) instead. Edit this block only where",
  "a board already has one.\n\n"
)
