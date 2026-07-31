# Resolve a bquoted expression the way blockr.core does and evaluate it.
eval_bquoted <- function(expr, df) {
  expr <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(expr, list(data = df))
}

# TRUE when the expression carries a `data` symbol that is NOT wrapped in the
# `.()` slot. Note that eval_bquoted() above passes either way -- the eval env
# binds `data` -- which is exactly why the defect is invisible at runtime and
# only breaks exported code. all.vars() cannot be used for this: the slot
# `.(data)` contains a `data` symbol too, so it reports both forms.
has_bare_data <- function(e) {
  if (is.name(e)) {
    return(identical(as.character(e), "data"))
  }
  if (!is.call(e) && !is.pairlist(e)) {
    return(FALSE)
  }
  # `.(data)` is the slot, not a bare reference.
  if (is.call(e) && identical(e[[1L]], as.name(".")) && length(e) == 2L) {
    return(FALSE)
  }
  # Single-bracket indexing keeps NULL elements (a function definition's
  # srcref is NULL when parsed without srcrefs, i.e. from an installed pkg).
  any(vapply(as.list(e), has_bare_data, logical(1L)))
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
    # A freshly added block has no labels; block_server feeds the expr
    # through exprs_to_lang(), which rejects a bare symbol — the expr must
    # be a call (regression: quote(data) crashed the block on add).
    expect_true(is.call(expr))
    expect_identical(blockr.core:::exprs_to_lang(expr), expr)
    expect_identical(eval_bquoted(expr, mtcars), mtcars)
  }
})

test_that("the emitted expression carries the data SLOT, never a bare `data`", {
  # `expr_type = "bquoted"` means only `.()` terms are substituted -- a free
  # `data` symbol survives into the exported script, resolves to `utils::data`
  # and takes every downstream block down. Both branches must emit `.(data)`.
  for (labels in list(list(), NULL, character(), list(mpg = "MPG"))) {
    expr <- blockr.extra:::make_labeler_expr(labels)
    expect_false(has_bare_data(expr))
  }

  slot <- call(".", as.name("data"))
  expect_identical(blockr.extra:::make_labeler_expr(list()), slot)
  expect_identical(
    blockr.extra:::make_labeler_expr(list(mpg = "MPG"))[[2L]], slot
  )

  # The guard must actually be able to fail -- the pre-fix shape.
  expect_true(
    has_bare_data(quote(blockr.extra::set_column_labels(data, c(mpg = "MPG"))))
  )
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
