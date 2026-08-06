#' Minidag board extension
#'
#' A compact, list-shaped alternative to the blockr.dag workflow canvas
#' (`blockr.dag::new_dag_extension()`): blocks appear as rows in topological
#' order with a
#' git-commit-graph style rail drawn in the left gutter. Linear pipelines read
#' as a plain list; branches occupy extra rail lanes. The extension is a
#' drop-in replacement for the DAG extension in a
#' [blockr.dock::new_dock_board()]:
#'
#' - drag a block's rail dot onto another row (or dot) to connect; input
#'   arity is enforced the same way the DAG canvas does it (data blocks
#'   accept no inputs, transform blocks one, n-ary blocks fill their named
#'   slots, variadic blocks never fill up),
#' - release the drag on empty space to append a new block (opens the
#'   block browser, wired from the drag source),
#' - click a row to reveal that block's panel, double-click to rename,
#' - click a dot (or hover a rail edge) to inspect and remove connections,
#' - board stacks show as named frames; collapse them to a single row,
#' - block eval status (waiting / unset / failed) shows as a coloured dot
#'   per row, identical in meaning to the DAG node badge.
#'
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @return A `minidag_extension` object as constructed by
#' [blockr.dock::new_dock_extension()].
#'
#' @export
new_minidag_extension <- function(...) {
  blockr.dock::new_dock_extension(
    minidag_ext_srv,
    minidag_ext_ui,
    name = "Mini deck",
    description = paste(
      "Compact list-shaped workflow editor: blocks as rows in topological",
      "order with a commit-graph rail for the connections. Supports the",
      "same operations as the DAG canvas (connect respecting block input",
      "arity, append, remove, stacks, status indicators) in a fraction of",
      "the space."
    ),
    class = "minidag_extension",
    ...
  )
}

minidag_ext_ui <- function(id, board, ...) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    minidag_js_dep(),
    htmltools::div(
      id = ns("deck"),
      class = "minidag",
      `data-ns` = ns("")
    )
  )
}

#' HTML dependency for the minidag rail renderer
#'
#' The list+rail editor *without* the board adapter: `minidagRail.create(el,
#' adapter)` plus the stylesheet, for a host that drives it with an adapter of
#' its own. blockr.process uses this to edit a process definition -- its nodes
#' are steps and its edges are dependencies -- with the same rows, rail,
#' dots and gestures the board editor uses.
#'
#' See the header of `inst/js/minidag-rail.js` for the adapter contract.
#'
#' @return An [htmltools::htmlDependency()] list.
#'
#' @export
minidag_rail_dep <- memoise0(function() {
  htmltools::tagList(
    minidag_css_dep(),
    htmltools::htmlDependency(
      name = "minidag-rail",
      version = as.character(utils::packageVersion("blockr.extra")),
      src = system.file("js", package = "blockr.extra"),
      # order is load-bearing: `minidag-rail.js` reads `minidagLayout` off
      # the global
      script = c("minidag-layout.js", "minidag-rail.js")
    )
  )
})

minidag_js_dep <- memoise0(function() {
  htmltools::tagList(
    minidag_rail_dep(),
    htmltools::htmlDependency(
      name = "minidag",
      version = as.character(utils::packageVersion("blockr.extra")),
      src = system.file("js", package = "blockr.extra"),
      # the board adapter, which reads `minidagRail` off the global
      script = "minidag.js"
    )
  )
})

minidag_css_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "minidag-css",
    version = as.character(utils::packageVersion("blockr.extra")),
    src = system.file("css", package = "blockr.extra"),
    stylesheet = "minidag.css"
  )
})

# The full board model as one JSON-ready payload. Pushed wholesale on every
# board change: the deck is a stateless list (no user-owned positions to
# preserve), so a full re-render is both cheap and always consistent --
# no delta bookkeeping as in blockr.dag's incremental g6 proxy.
minidag_payload <- function(board) {

  blocks <- blockr.core::board_blocks(board)
  links <- blockr.core::board_links(board)
  stacks <- blockr.core::board_stacks(board)

  meta <- if (length(blocks)) blockr.dock::blks_metadata(blocks)

  blk_entry <- function(i) {
    b <- blocks[[i]]
    list(
      id = names(blocks)[i],
      name = blockr.core::block_name(b),
      category = meta$category[i],
      color = meta$color[i],
      icon = minidag_icon(meta$icon[i], meta$color[i]),
      inputs = I(as.list(blockr.core::block_inputs(b))),
      variadic = is.na(blockr.core::block_arity(b))
    )
  }

  lnk_entry <- function(i) {
    list(
      id = names(links)[i],
      from = links$from[i],
      to = links$to[i],
      input = links$input[i]
    )
  }

  stk_entry <- function(id) {
    s <- stacks[[id]]
    col <- attr(s, "color")
    list(
      id = id,
      name = blockr.core::stack_name(s),
      color = if (is.character(col)) col,
      blocks = I(as.list(blockr.core::stack_blocks(s)))
    )
  }

  list(
    blocks = lapply(seq_along(blocks), blk_entry),
    links = lapply(seq_along(links), lnk_entry),
    stacks = lapply(names(stacks), stk_entry)
  )
}

# `jsonlite::base64_enc()` line-wraps its output; browsers tolerate that in
# an <img src> but stripping is free insurance (CSS url() would not).
minidag_icon <- function(icon_svg, color) {
  uri <- tryCatch(
    blockr.dock::blk_icon_data_uri(icon_svg, color),
    error = function(e) NULL
  )
  if (is.null(uri)) {
    return(NULL)
  }
  gsub("[\r\n[:space:]]", "", uri)
}

# Reveal a block's panel in the *current* view: focus it if the view already
# holds it, otherwise add it there -- never switch to another view that
# happens to hold it (same semantics as blockr.dag's node click).
minidag_reveal_delta <- function(board, block) {

  views <- blockr.dock::board_views(board)
  view <- blockr.dock::active_view(views)

  if (is.null(view)) {
    return(NULL)
  }

  pid <- as.character(blockr.dock::as_block_panel_id(block))

  ops <- if (pid %in% blockr.dock::view_members(views[[view]])) {
    list(select = pid)
  } else {
    list(add = stats::setNames(list(list()), pid), select = pid)
  }

  list(views = list(mod = stats::setNames(list(ops), view)))
}

minidag_ext_result <- function(board, extensions) {
  extensions[[
    blockr.dock::extension_ids(
      shiny::isolate(board$board),
      "minidag_extension"
    )
  ]]
}

#' @importFrom blockr.dock extension_block_callback
#' @export
extension_block_callback.minidag_extension <- function(x, ...) {
  function(id, board, update, conditions, extensions, ...,
           session = shiny::getDefaultReactiveDomain()) {

    mini <- minidag_ext_result(board, extensions)

    badge <- shiny::reactive({
      errors <- sum(lengths(conditions()$error))
      status <- board$eval[[id]]
      if (is.function(status)) {
        status <- status()
      }
      list(
        spec = blockr.dock::block_status_badge(status, errors),
        status = if (errors > 0L) "failed" else status
      )
    })

    drawn <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      list(mini$ready(), badge()),
      {
        shiny::req(mini$ready())

        res <- badge()
        spec <- res$spec

        # `NA` means the block is dormant: its status is not currently
        # computed, so leave the last-known dot rather than clearing it.
        if (isTRUE(is.na(spec)) || identical(spec, drawn())) {
          return()
        }

        mini$send(
          "badge",
          if (is.null(spec)) {
            list(id = id)
          } else {
            list(
              id = id,
              color = spec$color,
              label = spec$label,
              status = res$status
            )
          }
        )

        drawn(spec)
      },
      label = paste0("minidag_badge_", id)
    )

    NULL
  }
}
