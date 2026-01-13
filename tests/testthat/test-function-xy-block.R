# Test function_xy_block

testServer <- shiny::testServer

# Sample data for testing
df1 <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  id = 1:3,
  score = c(85, 90, 78),
  stringsAsFactors = FALSE
)

test_that("new_function_xy_block creates a valid block", {
  block <- new_function_xy_block()

  expect_s3_class(block, "function_xy_block")
  expect_s3_class(block, "block")
})

test_that("new_function_xy_block accepts custom functions", {
  my_fn <- function(x, y) {
    merge(x, y, by = "id")
  }

  block <- new_function_xy_block(fn = my_fn)

  expect_s3_class(block, "function_xy_block")
})

test_that("function_xy_block produces correct result with default function", {
  block <- new_function_xy_block()  # Default is bind_rows(x, y)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 6L)  # 3 + 3 rows from bind_rows
    },
    args = list(
      x = block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})

test_that("function_xy_block produces correct result with merge function", {
  my_fn <- function(x, y) {
    merge(x, y, by = "id")
  }

  block <- new_function_xy_block(fn = my_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
      expect_true("name" %in% colnames(result))
      expect_true("score" %in% colnames(result))
    },
    args = list(
      x = block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})

test_that("function_xy_block with parameters produces correct result", {
  merge_fn <- function(x, y, by = "id", all = FALSE) {
    merge(x, y, by = by, all = all)
  }

  block <- new_function_xy_block(fn = merge_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(is.data.frame(result))
      expect_true(nrow(result) >= 0)
    },
    args = list(
      x = block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})

test_that("function_xy_block auto-detects GT output", {
  # Function that returns a gt object
  gt_fn <- function(x, y) {
    merged <- merge(x, y, by = "id")
    gt::gt(merged)
  }

  block <- new_function_xy_block(fn = gt_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "gt_tbl")
    },
    args = list(
      x = block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})

test_that("function_xy_block can be reconstructed from function text", {
  # This tests the serialization/restoration pattern
  original_fn <- function(x, y) {
    merge(x, y, by = "id")
  }

  # Create block and get the function as text (how it would be serialized)
  fn_text <- paste(deparse(original_fn), collapse = "\n")

  # Reconstruct function from text (how it would be restored)
  restored_fn <- eval(parse(text = fn_text))

  # Create new block from restored function
  restored_block <- new_function_xy_block(fn = restored_fn)

  # Verify the restored block works correctly
  testServer(
    blockr.core::get_s3_method("block_server", restored_block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(
      x = restored_block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})

test_that("function_xy_block handles function with no extra parameters", {
  simple_fn <- function(x, y) {
    merge(x, y, by = "id")
  }

  block <- new_function_xy_block(fn = simple_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(
      x = block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})

test_that("function_xy_block handles different parameter types", {
  # Test numeric, logical, and character parameters
  typed_fn <- function(
    x, y,
    n = 2L,                    # numeric
    all_x = TRUE,              # logical
    suffix = "_merged"         # character
  ) {
    merged <- merge(x, y, by = "id", all.x = all_x)
    utils::head(merged, n)
  }

  block <- new_function_xy_block(fn = typed_fn)

  # Just verify the block can be created and produces output
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 2L)  # n = 2 default
    },
    args = list(
      x = block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})

test_that("function_xy_block with dplyr left_join works", {
  join_fn <- function(x, y) {
    dplyr::left_join(x, y, by = "id")
  }

  block <- new_function_xy_block(fn = join_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
      expect_true("name" %in% colnames(result))
      expect_true("score" %in% colnames(result))
    },
    args = list(
      x = block,
      data = list(x = function() df1, y = function() df2)
    )
  )
})
