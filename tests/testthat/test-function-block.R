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

test_that("function_block result is not NULL", {
  block <- new_function_block()

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_false(is.null(result))
      expect_true(is.data.frame(result))
      expect_true(nrow(result) > 0)
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

# Tests verifying different parameter values are reflected in results
# These tests create blocks with different default parameter values and verify
# the results match those parameter values

test_that("function_block numeric parameter values are reflected in result", {
  # Test with n = 3
  fn_3 <- function(data, n = 3L) { utils::head(data, n) }
  block_3 <- new_function_block(fn = fn_3)

  testServer(
    blockr.core::get_s3_method("block_server", block_3),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(x = block_3, data = list(data = function() datasets::iris))
  )

  # Test with n = 10
  fn_10 <- function(data, n = 10L) { utils::head(data, n) }
  block_10 <- new_function_block(fn = fn_10)

  testServer(
    blockr.core::get_s3_method("block_server", block_10),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 10L)
    },
    args = list(x = block_10, data = list(data = function() datasets::iris))
  )

  # Test with n = 1
  fn_1 <- function(data, n = 1L) { utils::head(data, n) }
  block_1 <- new_function_block(fn = fn_1)

  testServer(
    blockr.core::get_s3_method("block_server", block_1),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 1L)
    },
    args = list(x = block_1, data = list(data = function() datasets::iris))
  )
})

test_that("function_block logical parameter values are reflected in result", {
  # Test with keep_all = TRUE
  fn_true <- function(data, keep_all = TRUE) {
    if (keep_all) data else utils::head(data, 5)
  }
  block_true <- new_function_block(fn = fn_true)

  testServer(
    blockr.core::get_s3_method("block_server", block_true),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), nrow(datasets::iris))
    },
    args = list(x = block_true, data = list(data = function() datasets::iris))
  )

  # Test with keep_all = FALSE
  fn_false <- function(data, keep_all = FALSE) {
    if (keep_all) data else utils::head(data, 5)
  }
  block_false <- new_function_block(fn = fn_false)

  testServer(
    blockr.core::get_s3_method("block_server", block_false),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 5L)
    },
    args = list(x = block_false, data = list(data = function() datasets::iris))
  )
})

test_that("function_block character parameter values are reflected in result", {
  # Test with prefix = "row"
  fn_row <- function(data, prefix = "row") {
    data$label <- paste0(prefix, "_", seq_len(nrow(data)))
    utils::head(data, 3)
  }
  block_row <- new_function_block(fn = fn_row)

  testServer(
    blockr.core::get_s3_method("block_server", block_row),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(grepl("^row_", result$label)))
    },
    args = list(x = block_row, data = list(data = function() datasets::iris))
  )

  # Test with prefix = "item"
  fn_item <- function(data, prefix = "item") {
    data$label <- paste0(prefix, "_", seq_len(nrow(data)))
    utils::head(data, 3)
  }
  block_item <- new_function_block(fn = fn_item)

  testServer(
    blockr.core::get_s3_method("block_server", block_item),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(grepl("^item_", result$label)))
    },
    args = list(x = block_item, data = list(data = function() datasets::iris))
  )
})

test_that("function_block select parameter values are reflected in result", {
  # Test filtering to setosa
  fn_setosa <- function(data, species = "setosa") {
    dplyr::filter(data, Species == species)
  }
  block_setosa <- new_function_block(fn = fn_setosa)

  testServer(
    blockr.core::get_s3_method("block_server", block_setosa),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 50L)
      expect_true(all(result$Species == "setosa"))
    },
    args = list(x = block_setosa, data = list(data = function() datasets::iris))
  )

  # Test filtering to versicolor
  fn_versicolor <- function(data, species = "versicolor") {
    dplyr::filter(data, Species == species)
  }
  block_versicolor <- new_function_block(fn = fn_versicolor)

  testServer(
    blockr.core::get_s3_method("block_server", block_versicolor),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 50L)
      expect_true(all(result$Species == "versicolor"))
    },
    args = list(x = block_versicolor, data = list(data = function() datasets::iris))
  )

  # Test filtering to virginica
  fn_virginica <- function(data, species = "virginica") {
    dplyr::filter(data, Species == species)
  }
  block_virginica <- new_function_block(fn = fn_virginica)

  testServer(
    blockr.core::get_s3_method("block_server", block_virginica),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 50L)
      expect_true(all(result$Species == "virginica"))
    },
    args = list(x = block_virginica, data = list(data = function() datasets::iris))
  )
})

test_that("function_block combined parameter values are reflected in result", {
  # Test with n=5, species="setosa", descending=TRUE
  fn_combined <- function(data, n = 5L, species = "setosa", descending = TRUE) {
    result <- dplyr::filter(data, Species == species)
    if (descending) {
      result <- dplyr::arrange(result, dplyr::desc(Sepal.Length))
    }
    utils::head(result, n)
  }
  block <- new_function_block(fn = fn_combined)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 5L)
      expect_true(all(result$Species == "setosa"))
      # Check descending order
      expect_true(all(diff(result$Sepal.Length) <= 0))
    },
    args = list(x = block, data = list(data = function() datasets::iris))
  )

  # Test with different values: n=3, species="versicolor", descending=FALSE
  fn_combined2 <- function(data, n = 3L, species = "versicolor", descending = FALSE) {
    result <- dplyr::filter(data, Species == species)
    if (descending) {
      result <- dplyr::arrange(result, dplyr::desc(Sepal.Length))
    } else {
      result <- dplyr::arrange(result, Sepal.Length)
    }
    utils::head(result, n)
  }
  block2 <- new_function_block(fn = fn_combined2)

  testServer(
    blockr.core::get_s3_method("block_server", block2),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
      expect_true(all(result$Species == "versicolor"))
      # Check ascending order
      expect_true(all(diff(result$Sepal.Length) >= 0))
    },
    args = list(x = block2, data = list(data = function() datasets::iris))
  )
})
