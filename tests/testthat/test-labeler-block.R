# Resolve a bquoted expression the way blockr.core does and evaluate it.
eval_bquoted <- function(expr, df) {
  expr <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(expr, list(data = df))
}

# --- set_column_labels() ------------------------------------------------

test_that("set_column_labels sets, overwrites and removes labels", {
  res <- set_column_labels(mtcars, c(mpg = "Miles per gallon"))
  expect_identical(attr(res$mpg, "label"), "Miles per gallon")

  # everything else untouched
  expect_null(attr(res$cyl, "label"))
  expect_identical(res$mpg, `attr<-`(mtcars$mpg, "label", "Miles per gallon"))

  # overwrite an existing label
  res <- set_column_labels(res, c(mpg = "MPG"))
  expect_identical(attr(res$mpg, "label"), "MPG")

  # empty string removes the label
  res <- set_column_labels(res, c(mpg = ""))
  expect_null(attr(res$mpg, "label"))
})

test_that("set_column_labels accepts named lists and skips absent columns", {
  res <- set_column_labels(
    mtcars,
    list(mpg = "Miles per gallon", not_a_col = "Ghost")
  )
  expect_identical(attr(res$mpg, "label"), "Miles per gallon")
  expect_false("not_a_col" %in% names(res))

  # no labels is a no-op
  expect_identical(set_column_labels(mtcars, character()), mtcars)
  expect_identical(set_column_labels(mtcars, list()), mtcars)
})

# --- make_labeler_expr() ------------------------------------------------

test_that("make_labeler_expr builds a set_column_labels call", {
  expr <- blockr.extra:::make_labeler_expr(list(mpg = "Miles per gallon"))
  expect_true(is.call(expr))

  res <- eval_bquoted(expr, mtcars)
  expect_identical(attr(res$mpg, "label"), "Miles per gallon")
})

test_that("make_labeler_expr with no labels is the identity", {
  for (labels in list(list(), NULL, character())) {
    expr <- blockr.extra:::make_labeler_expr(labels)
    expect_identical(eval_bquoted(expr, mtcars), mtcars)
  }
})

test_that("make_labeler_expr drops malformed entries", {
  expr <- blockr.extra:::make_labeler_expr(
    list(mpg = "MPG", "unnamed", cyl = c("too", "long"), disp = NA)
  )
  res <- eval_bquoted(expr, mtcars)
  expect_identical(attr(res$mpg, "label"), "MPG")
  expect_null(attr(res$cyl, "label"))
  expect_null(attr(res$disp, "label"))
})

# --- block construction -------------------------------------------------

test_that("new_labeler_block constructs a transform block", {
  blk <- new_labeler_block(labels = list(mpg = "Miles per gallon"))
  expect_s3_class(blk, "labeler_block")
  expect_s3_class(blk, "transform_block")
  expect_setequal(blockr.core::block_inputs(blk), "data")
})

test_that("labeler block validates its input", {
  blk <- new_labeler_block()
  expect_error(
    blockr.core::validate_data_inputs(blk, list(data = "not a df")),
    "data frame"
  )
  expect_null(blockr.core::validate_data_inputs(blk, list(data = mtcars)))
})

# --- server behavior ----------------------------------------------------

test_that("labeler block server exposes labels state and expr", {
  blk <- new_labeler_block(labels = list(mpg = "Miles per gallon"))

  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() mtcars)),
    {
      session$flushReact()

      expect_identical(
        session$returned$state$labels(),
        list(mpg = "Miles per gallon")
      )

      res <- eval_bquoted(session$returned$expr(), mtcars)
      expect_identical(attr(res$mpg, "label"), "Miles per gallon")

      # external control: set the field directly
      session$returned$state$labels(list(cyl = "Cylinders"))
      session$flushReact()

      res <- eval_bquoted(session$returned$expr(), mtcars)
      expect_null(attr(res$mpg, "label"))
      expect_identical(attr(res$cyl, "label"), "Cylinders")
    }
  )
})
