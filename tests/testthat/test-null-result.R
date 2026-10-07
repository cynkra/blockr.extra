# A NULL result must not paint the word NULL.
#
# blockr.core hands a block a NULL result whenever it is momentarily outside
# the eval set (see render_dynamic_output). The card is on screen while that
# happens, so what the renderer does with NULL is what the user sees between
# two paints of the real output.

test_that("render_dynamic_output() paints nothing for a NULL result", {
  shiny::testServer(
    function(input, output, session) {
      output$result <- render_dynamic_output(NULL, NULL, session)
    },
    {
      expect_false(
        grepl("NULL", paste(as.character(output$result), collapse = ""),
              fixed = TRUE)
      )
    }
  )
})

test_that("a code block paints nothing for a NULL result", {
  blk <- new_code_block(script = "n <- 6\n\nutils::head(data, n)")
  shiny::testServer(
    function(input, output, session) {
      output$result <- blockr.core::block_output(blk, NULL, session)
    },
    {
      expect_false(
        grepl("NULL", paste(as.character(output$result), collapse = ""),
              fixed = TRUE)
      )
    }
  )
})

test_that("a real result still reaches its renderer", {
  blk <- new_code_block(script = "n <- 6\n\nutils::head(data, n)")
  shiny::testServer(
    function(input, output, session) {
      output$result <- blockr.core::block_output(blk, utils::head(mtcars, 2),
                                                 session)
    },
    {
      expect_true(nzchar(paste(as.character(output$result), collapse = "")))
    }
  )
})
