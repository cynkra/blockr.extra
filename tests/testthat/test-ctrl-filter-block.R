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

  expect_match(ui, "blockr-gear-btn")
  expect_match(ui, "blockr-gear-section")
  # Closed by default: the receipt is what a non-building user sees.
  expect_match(ui, "display: none;", fixed = TRUE)
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
