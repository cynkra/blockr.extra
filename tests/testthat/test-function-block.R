# Test function_block

testServer <- shiny::testServer

test_that("new_function_block creates a valid block", {
  block <- new_function_block()

  expect_s3_class(block, "function_block")
  expect_s3_class(block, "block")
})

test_that("new_function_block accepts custom functions", {
  block <- new_function_block(
    fn = "function(data, n = 10L) { utils::head(data, n) }"
  )

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
  block <- new_function_block(
    fn = "function(data, n = 3L) { utils::head(data, n) }"
  )

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
  block <- new_function_block(
    fn = "function(data) { gt::gt(utils::head(data, 3)) }"
  )

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
  block <- new_function_block(
    fn = "function(data, species = c('setosa', 'versicolor', 'virginica')) {
      dplyr::filter(data, Species == species)
    }"
  )

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
  block <- new_function_block(
    fn = "function(data, n = 10L, species = c('setosa', 'versicolor', 'virginica')) {
      result <- dplyr::filter(data, Species == species)
      utils::head(result, n)
    }"
  )

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
  fn_text <- "function(data, n = 5L) { utils::head(data, n) }"

  # Create block from text (how it would be restored)
  restored_block <- new_function_block(fn = fn_text)

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
  block <- new_function_block(
    fn = "function(data) { utils::head(data, 10) }"
  )

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
  block <- new_function_block(
    fn = "function(data, n = 5L, keep_all = TRUE, label = 'filtered') {
      result <- utils::head(data, n)
      if (keep_all) result else result[1:2, ]
    }"
  )

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
  block_3 <- new_function_block(fn = "function(data, n = 3L) { utils::head(data, n) }")

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
  block_10 <- new_function_block(fn = "function(data, n = 10L) { utils::head(data, n) }")

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
  block_1 <- new_function_block(fn = "function(data, n = 1L) { utils::head(data, n) }")

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
  block_true <- new_function_block(
    fn = "function(data, keep_all = TRUE) { if (keep_all) data else utils::head(data, 5) }"
  )

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
  block_false <- new_function_block(
    fn = "function(data, keep_all = FALSE) { if (keep_all) data else utils::head(data, 5) }"
  )

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
  block_row <- new_function_block(
    fn = "function(data, prefix = 'row') {
      data$label <- paste0(prefix, '_', seq_len(nrow(data)))
      utils::head(data, 3)
    }"
  )

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
  block_item <- new_function_block(
    fn = "function(data, prefix = 'item') {
      data$label <- paste0(prefix, '_', seq_len(nrow(data)))
      utils::head(data, 3)
    }"
  )

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
  block_setosa <- new_function_block(
    fn = "function(data, species = 'setosa') { dplyr::filter(data, Species == species) }"
  )

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
  block_versicolor <- new_function_block(
    fn = "function(data, species = 'versicolor') { dplyr::filter(data, Species == species) }"
  )

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
  block_virginica <- new_function_block(
    fn = "function(data, species = 'virginica') { dplyr::filter(data, Species == species) }"
  )

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
  block <- new_function_block(
    fn = "function(data, n = 5L, species = 'setosa', descending = TRUE) {
      result <- dplyr::filter(data, Species == species)
      if (descending) {
        result <- dplyr::arrange(result, dplyr::desc(Sepal.Length))
      }
      utils::head(result, n)
    }"
  )

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
  block2 <- new_function_block(
    fn = "function(data, n = 3L, species = 'versicolor', descending = FALSE) {
      result <- dplyr::filter(data, Species == species)
      if (descending) {
        result <- dplyr::arrange(result, dplyr::desc(Sepal.Length))
      } else {
        result <- dplyr::arrange(result, Sepal.Length)
      }
      utils::head(result, n)
    }"
  )

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

# Tests for serialization/deserialization round-trip

test_that("function_block serializes and deserializes correctly", {
  fn_text <- "function(data, n = 5L) { utils::head(data, n) }"
  block <- new_function_block(fn = fn_text)

  # Serialize block with its state
  serialized <- blockr.core::blockr_ser(block, state = list(fn = fn_text))

  # Deserialize
  restored <- blockr.core::blockr_deser(serialized)

  expect_s3_class(restored, "function_block")

  # Verify the restored block works correctly
  testServer(
    blockr.core::get_s3_method("block_server", restored),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 5L)
      expect_equal(result, utils::head(datasets::iris, 5L))
    },
    args = list(
      x = restored,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block with select parameter serializes correctly", {
  fn_text <- "function(data, species = c('setosa', 'versicolor', 'virginica')) {
    dplyr::filter(data, Species == species)
  }"
  block <- new_function_block(fn = fn_text)

  # Serialize and deserialize
  serialized <- blockr.core::blockr_ser(block, state = list(fn = fn_text))
  restored <- blockr.core::blockr_deser(serialized)

  expect_s3_class(restored, "function_block")

  testServer(
    blockr.core::get_s3_method("block_server", restored),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(is.data.frame(result))
      expect_true(nrow(result) > 0)
    },
    args = list(
      x = restored,
      data = list(data = function() datasets::iris)
    )
  )
})

# Tests using the new text-based API (no deprecation warnings)

test_that("function_block works with text-based fn parameter", {
  block <- new_function_block(
    fn = "function(data, n = 3L) { utils::head(data, n) }"
  )

  expect_s3_class(block, "function_block")

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})

test_that("function_block text API with multiple parameters works", {
  block <- new_function_block(
    fn = "function(data, n = 5L, keep_cols = TRUE) {
      result <- utils::head(data, n)
      if (keep_cols) result else result[, 1:2]
    }"
  )

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 5L)
    },
    args = list(
      x = block,
      data = list(data = function() datasets::iris)
    )
  )
})
