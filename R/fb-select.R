#' Shared `Blockr.Select` input for function-block parameters
#'
#' Function blocks auto-generate one field per function argument. For arguments
#' whose default implies a choice (a `list()` -> multi-select, a length > 1
#' vector -> single-select), we mount the same hand-rolled `Blockr.Select`
#' component used across blockr.dplyr, rather than a plain `shiny::selectInput`.
#'
#' `fb_select_deps()` bundles every asset the control needs; `fb_select_input()`
#' emits the container the `fb-select` JS input binding hydrates.
#'
#' @name fb-select
#' @keywords internal
NULL


#' HTML dependency for the fb-select input binding
#'
#' @return An [htmltools::htmlDependency()].
#' @keywords internal
fb_select_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-extra-fb-select",
    version = as.character(utils::packageVersion("blockr.extra")),
    src = system.file("js", package = "blockr.extra"),
    script = "fb-select.js"
  )
})


#' All assets needed for a function-block select field
#'
#' Bundles the shared `Blockr.Select` component (via
#' [blockr.dplyr::blockr_select_dep()], which also pulls in blockr-core.js) with
#' the fb-select input binding.
#'
#' @return An [htmltools::tagList()] of `htmlDependency` objects.
#' @keywords internal
fb_select_deps <- function() {
  htmltools::tagList(
    blockr.dplyr::blockr_select_dep(),
    fb_select_dep()
  )
}


#' Build `Blockr.Select` options from a choices vector
#'
#' Turns a (possibly named) vector into the `[{value, label}]` shape the JS
#' component expects. Names become labels; unnamed entries label as their value.
#'
#' @param choices A vector of choices, optionally named.
#' @return A list of `list(value =, label =)` records.
#' @noRd
fb_select_options <- function(choices) {
  values <- as.character(unname(choices))
  labels <- names(choices)

  # Unnamed choices carry no meaningful label — emit bare strings so the
  # component doesn't render a redundant "value value" pair.
  if (is.null(labels)) {
    return(as.list(values))
  }

  labels <- as.character(labels)
  unname(Map(
    function(v, l) if (l == "" || l == v) v else list(value = v, label = l),
    values, labels
  ))
}


#' Container for a shared-select function-block field
#'
#' Returns the label + empty container the `fb-select` binding hydrates into a
#' `Blockr.Select`. Wrap the result in `.block-input-wrapper` at the call site
#' (as with the other field types) so it participates in the params grid.
#'
#' @param input_id Fully namespaced Shiny input id (also the container DOM id).
#' @param label Field label shown above the control.
#' @param choices Choices vector (optionally named).
#' @param selected Initial selection (scalar for single, vector for multi).
#' @param multiple Whether to mount a multi-select.
#' @param placeholder Placeholder text for the empty control.
#' @return A Shiny tag.
#' @noRd
fb_select_input <- function(input_id, label, choices, selected,
                            multiple = FALSE, placeholder = NULL) {
  options <- fb_select_options(choices)
  selected <- as.character(selected)

  shiny::tagList(
    shiny::tags$label(label, class = "fb-field-label", `for` = input_id),
    shiny::div(
      id = input_id,
      class = "blockr-select-input",
      `data-multiple` = if (isTRUE(multiple)) "true" else "false",
      `data-options` = jsonlite::toJSON(options, auto_unbox = TRUE),
      `data-selected` = jsonlite::toJSON(selected, auto_unbox = !isTRUE(multiple)),
      `data-placeholder` = if (is.null(placeholder)) "" else placeholder
    )
  )
}
