# A composer table returned from a function or code block carries the block's
# input as `source_data`, so a drill on it can find the subjects behind a
# count (blockr.sandbox composer_drill_claims(), route B).

composed <- function(...) structure(list(...), class = c("composed_table", "composed"))

test_that("a composer result carries the block's input", {
  dat <- data.frame(USUBJID = c("01", "02"), AEBODSYS = "GI")
  out <- stamp_source_data(composed(), list(data = dat))
  expect_identical(attr(out, "source_data"), dat)

  env <- new.env()
  env$data <- dat
  expect_identical(attr(stamp_source_data(composed(), env), "source_data"), dat)
})

test_that("a script's own stamp and other results are left alone", {
  dat <- data.frame(USUBJID = c("01", "02"))
  own <- composed()
  attr(own, "source_data") <- dat[1, , drop = FALSE]
  expect_identical(stamp_source_data(own, list(data = dat)), own)

  # composer's list-element form counts as a stamp too
  el <- composed(source_data = dat[1, , drop = FALSE])
  expect_null(attr(stamp_source_data(el, list(data = dat)), "source_data"))

  expect_identical(stamp_source_data(dat, list(data = dat)), dat)
  # a dm input (not a frame) is not stamped
  expect_null(attr(stamp_source_data(composed(), list(data = list(a = 1))),
                   "source_data"))
})

test_that("the function block stamps on evaluation", {
  blk <- new_function_block("function(data) data")
  dat <- data.frame(USUBJID = "01")
  expr <- quote(structure(list(), class = c("composed_table", "composed")))
  out <- blockr.core::block_eval(blk, expr, list(data = dat))
  expect_identical(attr(out, "source_data"), dat)
})
