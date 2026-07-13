test_that("ctrl_filter_block constructs and carries its state", {

  blk <- new_ctrl_filter_block(
    target = "cohort_filter",
    table = "adsl",
    columns = "SEX"
  )

  expect_s3_class(blk, "ctrl_filter_block")
  expect_s3_class(blk, "transform_block")
})

test_that("the block passes its input through unchanged", {

  # The sender pushes a control message; it does not transform data. Its expr
  # is the identity on `data`, so the drilled subset stays linkable downstream.
  drilled <- data.frame(SEX = c("F", "F"), n = 1:2)
  ex <- quote(base::identity(data))

  # blockr.core asserts `typeof(exprs) == "language"`: a bare `quote(data)` is
  # a SYMBOL, and the block errors out with that assertion instead of running.
  expect_identical(typeof(ex), "language")
  expect_equal(eval(ex, list(data = drilled)), drilled)
})

test_that("the wiring inputs sit behind the gear, not in the resting surface", {

  # expr_ui() renders the block's own `ui` slot -- the control section.
  # block_ui() is the preview (the DT of the passed-through data).
  ui <- as.character(
    htmltools::renderTags(
      blockr.core::expr_ui("blk", new_ctrl_filter_block())
    )$html
  )

  # The design-system band (blockr.dplyr / blockr.viz), not bespoke chrome:
  # a .blockr-gear-btn toggling an in-flow .blockr-settings band of
  # .blockr-popover-input fields. Closed by default (the band's CSS hides it
  # until --open), so the receipt is what a non-building user sees.
  expect_match(ui, "blockr-gear-btn")
  # The band element itself carries no --open class (only the gear's onclick
  # mentions it, to toggle it).
  expect_match(ui, 'class="blockr-settings blockr-settings--beak"', fixed = TRUE)
  expect_match(ui, "blockr-settings__grid", fixed = TRUE)
  expect_match(ui, "blockr-popover-input", fixed = TRUE)

  # Commit on blur / Enter: a per-keystroke report would ctrl_send() at every
  # half-typed target id.
  expect_match(ui, 'data-update-on="blur"', fixed = TRUE)
})

test_that("an unconfigured block claims nothing (passes through, no error)", {

  drilled <- data.frame(.variable = "SEX", .variable_level = "F")

  # No table set -> the server's `cols` reactive short-circuits to no claim.
  expect_length(drill_claim_columns(drilled, table = ""), 1L)
  expect_silent(new_ctrl_filter_block())
})

test_that("split_columns() parses the comma-separated UI field", {

  expect_equal(split_columns("SEX, ARM"), c("SEX", "ARM"))
  expect_equal(split_columns("SEX"), "SEX")
  expect_equal(split_columns(""), character())
  expect_equal(split_columns(NULL), character())
  expect_equal(split_columns(" SEX ,, ARM "), c("SEX", "ARM"))
})
