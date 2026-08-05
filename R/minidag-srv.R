# Minidag extension server. The JS side announces itself via the `ready`
# input (the panel UI is moved into its dock panel after page load, so the
# client, not the server, knows when it can render); from then on every
# board change pushes the full model. User gestures come back as
# event-priority inputs and are translated into blockr.core `update()`
# deltas or blockr.dock action triggers.
minidag_ext_srv <- function(id, board, update, actions, ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      send <- function(type, payload) {
        payload$el <- session$ns("deck")
        session$sendCustomMessage(paste0("minidag-", type), payload)
      }

      ready <- shiny::reactive(isTRUE(input$ready))

      shiny::observeEvent(
        list(board$board, input$ready),
        {
          shiny::req(isTRUE(input$ready))
          send("data", minidag_payload(board$board))
        }
      )

      # Connect: the client proposes an input slot (it knows the free
      # slots), the server re-derives the free set from the committed
      # board and only honours the proposal when it is (still) free --
      # the authoritative gate, same as blockr.dag's draw_link_action.
      shiny::observeEvent(input$link_add, {
        msg <- input$link_add
        blocks <- blockr.core::board_blocks(board$board)

        if (!all(c(msg$from, msg$to) %in% names(blocks))) {
          return()
        }

        inps <- blockr.dock::block_input_select(
          blocks[[msg$to]],
          msg$to,
          blockr.core::board_links(board$board),
          mode = "inputs"
        )

        if (length(inps) == 0L) {
          shiny::showNotification(
            sprintf("No free inputs on block %s.", msg$to),
            type = "warning"
          )
          return()
        }

        slot <- if (length(msg$input) == 1L && msg$input %in% inps) {
          msg$input
        } else {
          inps[1L]
        }

        update(list(links = list(add = blockr.core::as_links(
          blockr.core::new_link(from = msg$from, to = msg$to, input = slot)
        ))))
      })

      shiny::observeEvent(input$link_rm, {
        ids <- intersect(
          unlist(input$link_rm$ids),
          names(blockr.core::board_links(board$board))
        )
        if (length(ids)) {
          update(list(links = list(rm = ids)))
        }
      })

      # Remove a block; when it sits in a straight chain (exactly one
      # incoming and one outgoing link) the chain is healed by wiring the
      # parent into the freed slot of the child. `augment_board_update()`
      # cascades the incident link removals and stack pruning.
      shiny::observeEvent(input$block_rm, {
        id_rm <- input$block_rm$id

        if (!id_rm %in% names(blockr.core::board_blocks(board$board))) {
          return()
        }

        links <- blockr.core::board_links(board$board)
        ins <- links[links$to == id_rm]
        outs <- links[links$from == id_rm]

        upd <- list(blocks = list(rm = id_rm))

        dup <- length(ins) == 1L && length(outs) == 1L && any(
          links$from == ins$from &
            links$to == outs$to &
            links$input == outs$input
        )

        if (length(ins) == 1L && length(outs) == 1L && !dup) {
          upd$links <- list(add = blockr.core::as_links(
            blockr.core::new_link(
              from = ins$from,
              to = outs$to,
              input = outs$input
            )
          ))
        }

        update(upd)
      })

      shiny::observeEvent(input$block_rename, {
        msg <- input$block_rename
        nm <- trimws(as.character(msg$name))
        if (!nzchar(nm)) {
          return()
        }
        update(list(blocks = list(
          mod = stats::setNames(list(list(block_name = nm)), msg$id)
        )))
      })

      shiny::observeEvent(input$block_select, {
        delta <- minidag_reveal_delta(board$board, input$block_select$id)
        if (!is.null(delta)) {
          update(delta)
        }
      })

      # Drag released on empty canvas: open the block browser, wired from
      # the drag source (the deck's drop-on-canvas append, same flow the
      # DAG extension triggers for an edge dropped on the canvas).
      shiny::observeEvent(input$block_append, {
        actions[["append_block_action"]](input$block_append$from)
      })

      shiny::observeEvent(input$block_add, {
        actions[["add_block_action"]](input$block_add)
      })

      shiny::observeEvent(input$stack_add, {
        members <- unlist(input$stack_add$blocks)
        members <- intersect(
          members,
          names(blockr.core::board_blocks(board$board))
        )
        if (length(members) < 2L) {
          return()
        }
        update(list(stacks = list(add = blockr.core::stacks(
          blockr.dock::new_dock_stack(blocks = members, name = "New stack")
        ))))
      })

      shiny::observeEvent(input$stack_rename, {
        msg <- input$stack_rename
        nm <- trimws(as.character(msg$name))
        if (!nzchar(nm)) {
          return()
        }
        update(list(stacks = list(
          mod = stats::setNames(list(list(name = nm)), msg$id)
        )))
      })

      shiny::observeEvent(input$stack_rm, {
        id_rm <- input$stack_rm$id
        if (id_rm %in% names(blockr.core::board_stacks(board$board))) {
          update(list(stacks = list(rm = id_rm)))
        }
      })

      list(
        state = list(),
        ready = ready,
        send = send
      )
    }
  )
}
