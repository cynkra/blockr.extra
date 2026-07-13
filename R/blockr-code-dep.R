#' HTML dependency for the Blockr.Code editor (CodeMirror 6 bundle)
#'
#' Ships `inst/js/blockr-code.js` (built from `srcjs/code-block/` with esbuild;
#' see `package.json`). The bundle registers the `blockr.extra.code` Shiny input
#' binding, the `blockr-code-set` custom-message handler, and `Blockr.Code.refresh`.
#'
#' @return An [htmltools::htmlDependency()].
#' @keywords internal
blockr_code_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-code",
    version = as.character(utils::packageVersion("blockr.extra")),
    src = system.file("js", package = "blockr.extra"),
    script = "blockr-code.js"
  )
})

#' @rdname blockr_code_dep
#' @keywords internal
code_block_css_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-code-css",
    version = as.character(utils::packageVersion("blockr.extra")),
    src = system.file("css", package = "blockr.extra"),
    stylesheet = "code-block.css"
  )
})
