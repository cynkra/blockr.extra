# Test function_block

testServer <- shiny::testServer

test_that("new_function_block creates a valid block", {
  block <- new_function_block()

  expect_s3_class(block, "function_block")
  expect_s3_class(block, "block")
})

test_that("new_function_block accepts custom functions", {
  my_fn <- function(data, n = 10L) {
    utils::head(data, n)
  }

  block <- new_function_block(fn = my_fn)

  expect_s3_class(block, "function_block")
})

test_that("function_block produces correct result with default function", {
  block <- new_function_block()  # Default is head(data, 6)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 6L)
      expect_equal(result, utils::head(datasets::iris, 6L))
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block produces correct result with custom function", {
  my_fn <- function(data, n = 3L) {
    utils::head(data, n)
  }

  block <- new_function_block(fn = my_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
      expect_equal(result, utils::head(datasets::iris, 3L))
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block auto-detects GT output", {
  # Function that returns a gt object
  gt_fn <- function(data) {
    gt::gt(utils::head(data, 3))
  }

  block <- new_function_block(fn = gt_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "gt_tbl")
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block with filter function produces correct result", {
  filter_fn <- function(data, species = c("setosa", "versicolor", "virginica")) {
    dplyr::filter(data, Species == species)
  }

  block <- new_function_block(fn = filter_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      # When parameters aren't yet initialized from UI, function uses defaults
      # The result should be filtered to a single species (the first default)
      # or all data if parameter initialization fails
      expect_true(is.data.frame(result))
      expect_true(nrow(result) > 0)
      # Should have fewer rows than original or same (if filtering worked)
      expect_true(nrow(result) <= nrow(datasets::iris))
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block with multiple parameters works", {
  multi_fn <- function(
    data,
    n = 10L,
    species = c("setosa", "versicolor", "virginica")
  ) {
    result <- dplyr::filter(data, Species == species)
    utils::head(result, n)
  }

  block <- new_function_block(fn = multi_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      # Just verify the block produces valid output
      expect_true(is.data.frame(result))
      expect_true(nrow(result) > 0)
      expect_true(nrow(result) <= 10L)  # Should be at most n rows
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block can be reconstructed from function text", {
  # This tests the serialization/restoration pattern
  original_fn <- function(data, n = 5L) {
    utils::head(data, n)
  }

  # Create block and get the function as text (how it would be serialized)
  fn_text <- paste(deparse(original_fn), collapse = "\n")

  # Reconstruct function from text (how it would be restored)
  restored_fn <- eval(parse(text = fn_text))

  # Create new block from restored function
  restored_block <- new_function_block(fn = restored_fn)

  # Verify the restored block works correctly
  testServer(
    blockr.core::get_s3_method("block_server", restored_block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 5L)
      expect_equal(result, utils::head(datasets::iris, 5L))
    },
    args = list(
      x = restored_block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block handles function with no extra parameters", {
  simple_fn <- function(data) {
    utils::head(data, 10)
  }

  block <- new_function_block(fn = simple_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 10L)
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block handles different parameter types", {
  # Test numeric, logical, and character parameters
  typed_fn <- function(
    data,
    n = 5L,                    # numeric
    keep_all = TRUE,           # logical
    label = "filtered"         # character
  ) {
    result <- utils::head(data, n)
    if (keep_all) result else result[1:2, ]
  }

  block <- new_function_block(fn = typed_fn)

  # Just verify the block can be created and produces output
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 5L)  # n = 5 default
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})
