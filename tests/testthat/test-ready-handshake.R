# A block on a dock panel that is not on the startup view has its server run
# at boot but its JavaScript delivered later, with the panel. The boot pushes
# are dropped (Shiny discards custom messages with no handler), so the client
# announces itself on bind and R re-sends. Both blocks here register through
# blockr.dplyr's `Blockr.registerBlock()`, which does the announcing.
# See blockr.core#317 and blockr.dplyr's test-ready-handshake.R.

# Capture custom messages. Assigning into `session$rootScope()$...` errors on
# the proxy session, so the root MockShinySession has to be bound first.
capture_messages <- function(session) {
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  root <- session$rootScope()
  root$sendCustomMessage <- function(type, message) {
    sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
    invisible(NULL)
  }
  sent
}

types_of <- function(sent) vapply(sent$msgs, `[[`, character(1L), "type")

msg_of <- function(sent, type) sent$msgs[[which(types_of(sent) == type)]]$message

test_that("a client announcing itself gets the labeler state re-sent", {
  blk <- new_labeler_block(labels = list(mpg = "Miles per gallon"))

  shiny::testServer(blk$expr_server, args = list(data = shiny::reactive(mtcars)), {
    session$flushReact()
    sent <- capture_messages(session)

    session$setInputs(labeler_input_ready = 1)
    session$flushReact()

    expect_setequal(types_of(sent), c("labeler-block-update", "labeler-columns"))

    state <- msg_of(sent, "labeler-block-update")
    expect_equal(state$state$labels$mpg, "Miles per gallon")

    cols <- msg_of(sent, "labeler-columns")
    expect_true(length(cols$columns) > 0L)
  })
})

test_that("the labeler announce re-sends state while the upstream is unset", {
  blk <- new_labeler_block(labels = list(mpg = "Miles per gallon"))

  shiny::testServer(blk$expr_server, args = list(data = shiny::reactive(shiny::req(FALSE))), {
    session$flushReact()
    sent <- capture_messages(session)

    session$setInputs(labeler_input_ready = 1)
    session$flushReact()

    # No columns to send, but the state must still get through: the state push
    # does not touch `data()`, so an unset upstream cannot leave the block
    # blank.
    expect_equal(types_of(sent), "labeler-block-update")
    expect_equal(msg_of(sent, "labeler-block-update")$state$labels$mpg,
                 "Miles per gallon")
  })
})

test_that("a client announcing itself gets the compare state re-sent", {
  blk <- new_compare_block(
    key_cols = "cyl",
    measure_cols = "mpg",
    join_type = "full",
    metric = "rel_diff"
  )

  shiny::testServer(
    blk$expr_server,
    args = list(x = shiny::reactive(mtcars), y = shiny::reactive(mtcars)),
    {
      session$flushReact()
      sent <- capture_messages(session)

      session$setInputs(compare_input_ready = 1)
      session$flushReact()

      expect_setequal(
        types_of(sent),
        c("compare-block-update", "compare-columns")
      )

      state <- msg_of(sent, "compare-block-update")$state
      expect_equal(state$key_cols, list("cyl"))
      expect_equal(state$measure_cols, list("mpg"))
      # The JS defaults are "inner"/"diff" -- a dropped push is what leaves
      # the client showing those instead of the restored pair.
      expect_equal(state$join_type, "full")
      expect_equal(state$metric, "rel_diff")

      cols <- msg_of(sent, "compare-columns")
      expect_true(length(cols$columns) > 0L)
    }
  )
})
