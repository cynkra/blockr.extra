# Tests for search block

testServer <- shiny::testServer

# --- make_search_expr() unit tests ---

test_that("empty string produces a passthrough expression", {
  passthrough <- quote(dplyr::filter(data))
  expect_identical(blockr.extra:::make_search_expr(""), passthrough)
  expect_identical(blockr.extra:::make_search_expr("   "), passthrough)
  expect_identical(blockr.extra:::make_search_expr("\t\n"), passthrough)

  # and evaluating it returns the data unchanged
  data <- datasets::iris
  expect_equal(eval(passthrough), datasets::iris)
})

test_that("non-empty string produces a filter expression with the search string", {
  expr <- blockr.extra:::make_search_expr("BBD02")

  expect_true(is.call(expr))
  s <- deparse1(expr)
  expect_match(s, "dplyr::filter")
  expect_match(s, "dplyr::if_any")
  expect_match(s, "BBD02")
  expect_match(s, "ignore_case = TRUE")
})

test_that("search expression filters rows on substring match", {
  data <- data.frame(
    id   = c("ABC01", "DEF02", "bbd02"),
    note = c("alpha", "beta BBD02 mid", "gamma"),
    stringsAsFactors = FALSE
  )

  result <- eval(blockr.extra:::make_search_expr("BBD02"))

  # both the lowercase id and the mid-of-string note match (case-insensitive)
  expect_equal(nrow(result), 2L)
  expect_true(all(c("DEF02", "bbd02") %in% result$id))
})

test_that("search expression is case-insensitive", {
  data <- data.frame(x = c("Setosa", "SETOSA", "other"), stringsAsFactors = FALSE)

  result <- eval(blockr.extra:::make_search_expr("setosa"))

  expect_equal(nrow(result), 2L)
})

test_that("search expression handles NA cells without crashing", {
  data <- data.frame(
    x = c(NA, "foo", NA),
    y = c("bar", NA, NA),
    stringsAsFactors = FALSE
  )

  result <- eval(blockr.extra:::make_search_expr("foo"))

  expect_equal(nrow(result), 1L)
  expect_equal(result$x, "foo")
})

test_that("search expression matches across numeric, factor, and date columns", {
  data <- data.frame(
    n = c(12345, 67890),
    f = factor(c("alpha", "beta")),
    d = as.Date(c("2024-01-15", "2024-06-30")),
    stringsAsFactors = FALSE
  )

  # numeric match
  expect_equal(nrow(eval(blockr.extra:::make_search_expr("123"))), 1L)
  # factor match
  expect_equal(nrow(eval(blockr.extra:::make_search_expr("alpha"))), 1L)
  # date match (ISO format)
  expect_equal(nrow(eval(blockr.extra:::make_search_expr("2024-06"))), 1L)
})

test_that("search expression returns empty when nothing matches", {
  data <- data.frame(x = c("a", "b", "c"), stringsAsFactors = FALSE)

  result <- eval(blockr.extra:::make_search_expr("zzz"))

  expect_equal(nrow(result), 0L)
})

# --- Block construction tests ---

test_that("new_search_block creates a valid block with correct classes", {
  block <- new_search_block()

  expect_s3_class(block, "search_block")
  expect_s3_class(block, "transform_block")
  expect_s3_class(block, "block")
})

test_that("search block has arity 1 with data input", {
  block <- new_search_block()

  expect_equal(blockr.core::block_arity(block), 1L)
  expect_equal(blockr.core::block_inputs(block), "data")
})

test_that("search block rejects non-data-frame input via dat_valid", {
  block <- new_search_block()

  expect_error(
    blockr.core::validate_data_inputs(block, list(data = 1:10)),
    regexp = "data frame",
    ignore.case = TRUE
  )
})

# --- testServer behavior ---

test_that("search block returns unfiltered data when search is empty", {
  block <- new_search_block()

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), nrow(datasets::iris))
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("search block filters rows when initial string is non-empty", {
  block <- new_search_block(string = "setosa")

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(all(result$Species == "setosa"))
      expect_equal(nrow(result), 50L)
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("search block state is a writable reactive that drives result", {
  block <- new_search_block(string = "initial")

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(session$returned$state$string(), "initial")

      # Update state directly via the returned reactiveVal.
      # (session$setInputs does not reach the block's inner module namespace
      #  in testServer, so we exercise the state machinery directly.)
      session$returned$state$string("versicolor")
      session$flushReact()
      # Advance past the 300 ms debounce so the expression reactive updates.
      session$elapse(500)
      session$flushReact()

      expect_equal(session$returned$state$string(), "versicolor")
      result <- session$returned$result()
      expect_true(all(result$Species == "versicolor"))
      expect_equal(nrow(result), 50L)
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})
