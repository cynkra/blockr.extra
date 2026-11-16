# blockr.extra is being retired. Every block in it is deprecated in favour of
# a block in another package. The blocks stay so that saved boards keep
# loading, and they say so in three places: a line on the block above its
# result, a warning in the R log once per session, and "(deprecated)" in the
# block registry (R/registry.R).
#
# Line and warning come from the block's UI (block_ui methods), not from the
# constructors: the block list builds every registered block once at start,
# and a warning from there would name blocks no board uses.

# What replaces each deprecated block, as the line names it.
extra_replacements <- c(
  new_function_block       = "the Script block from blockr.script",
  new_code_block           = "the Script block from blockr.script",
  new_function_var_block   = "the Script block from blockr.script",
  new_function_xy_block    = "the Script block from blockr.script",
  new_async_function_block = "the Script block from blockr.script",
  new_broom_summary_block  = "the Broom block from blockr.stats",
  new_search_block         = "the Table block from blockr.viz (it has a search box)",
  new_labeler_block        = "the Script block from blockr.script",
  new_prose_block          = "the Prose block from blockr.script"
)

extra_deprecation_text <- function(ctor) {
  paste0(
    "Deprecated: use ", extra_replacements[[ctor]], ". ",
    "This block stays so that saved boards keep working."
  )
}

#' The line a deprecated block shows above its result
#' @param ctor The constructor's name, a name of `extra_replacements`.
#' @noRd
extra_deprecation_notice <- function(ctor) {
  shiny::div(
    class = "blockr-extra-deprecated",
    role = "note",
    style = paste(
      "font-size: var(--blockr-font-size-xs, 0.75rem);",
      "color: #92400e; background: #fef3c7; border: 1px solid #fcd34d;",
      "border-radius: 4px; padding: 4px 8px; margin: 0 0 8px;"
    ),
    extra_deprecation_text(ctor)
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
    ctor, "() is deprecated. Use ", extra_replacements[[ctor]], " instead.",
    use_glue = FALSE
  )
  invisible(NULL)
}

#' Notice plus the block's own UI, for a deprecated block's block_ui method
#' @noRd
extra_deprecated_ui <- function(ctor, ui) {
  warn_extra_deprecated(ctor)
  shiny::tagList(extra_deprecation_notice(ctor), ui)
}

# What the assistant reads about a deprecated block (R/registry.R).
extra_deprecation_guidance <- function(ctor) {
  paste0(
    "DEPRECATED: do not add this block to a board. Add ",
    extra_replacements[[ctor]], " instead. Edit this block only where a ",
    "board already has one.\n\n"
  )
}
