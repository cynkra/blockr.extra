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

test_that("function_xy_block result is not NULL", {
  block <- new_function_xy_block()

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

# Tests verifying different parameter values are reflected in results

test_that("function_xy_block numeric parameter values are reflected in result", {
  # Test with n = 1
  fn_1 <- function(x, y, n = 1L) {
    merged <- merge(x, y, by = "id")
    utils::head(merged, n)
  }
  block_1 <- new_function_xy_block(fn = fn_1)

  testServer(
    blockr.core::get_s3_method("block_server", block_1),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 1L)
    },
    args = list(x = block_1, data = list(x = function() df1, y = function() df2))
  )

  # Test with n = 2
  fn_2 <- function(x, y, n = 2L) {
    merged <- merge(x, y, by = "id")
    utils::head(merged, n)
  }
  block_2 <- new_function_xy_block(fn = fn_2)

  testServer(
    blockr.core::get_s3_method("block_server", block_2),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 2L)
    },
    args = list(x = block_2, data = list(x = function() df1, y = function() df2))
  )

  # Test with n = 3
  fn_3 <- function(x, y, n = 3L) {
    merged <- merge(x, y, by = "id")
    utils::head(merged, n)
  }
  block_3 <- new_function_xy_block(fn = fn_3)

  testServer(
    blockr.core::get_s3_method("block_server", block_3),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(x = block_3, data = list(x = function() df1, y = function() df2))
  )
})

test_that("function_xy_block logical parameter values are reflected in result", {
  # Create test data with non-matching ids
  df_x <- data.frame(id = 1:3, val_x = c("a", "b", "c"))
  df_y <- data.frame(id = 2:4, val_y = c("p", "q", "r"))

  # Test with all_rows = FALSE (inner join)
  fn_inner <- function(x, y, all_rows = FALSE) {
    merge(x, y, by = "id", all = all_rows)
  }
  block_inner <- new_function_xy_block(fn = fn_inner)

  testServer(
    blockr.core::get_s3_method("block_server", block_inner),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 2L)  # Only ids 2 and 3 match
    },
    args = list(x = block_inner, data = list(x = function() df_x, y = function() df_y))
  )

  # Test with all_rows = TRUE (outer join)
  fn_outer <- function(x, y, all_rows = TRUE) {
    merge(x, y, by = "id", all = all_rows)
  }
  block_outer <- new_function_xy_block(fn = fn_outer)

  testServer(
    blockr.core::get_s3_method("block_server", block_outer),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 4L)  # All ids 1, 2, 3, 4
    },
    args = list(x = block_outer, data = list(x = function() df_x, y = function() df_y))
  )
})

test_that("function_xy_block character parameter values are reflected in result", {
  # Test with suffix = "_combined"
  fn_combined <- function(x, y, suffix = "_combined") {
    merged <- merge(x, y, by = "id")
    names(merged) <- paste0(names(merged), suffix)
    merged
  }
  block_combined <- new_function_xy_block(fn = fn_combined)

  testServer(
    blockr.core::get_s3_method("block_server", block_combined),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(grepl("_combined$", names(result))))
    },
    args = list(x = block_combined, data = list(x = function() df1, y = function() df2))
  )

  # Test with suffix = "_joined"
  fn_joined <- function(x, y, suffix = "_joined") {
    merged <- merge(x, y, by = "id")
    names(merged) <- paste0(names(merged), suffix)
    merged
  }
  block_joined <- new_function_xy_block(fn = fn_joined)

  testServer(
    blockr.core::get_s3_method("block_server", block_joined),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(grepl("_joined$", names(result))))
    },
    args = list(x = block_joined, data = list(x = function() df1, y = function() df2))
  )
})

test_that("function_xy_block select parameter values are reflected in result", {
  # Create test data with non-matching ids
  df_x <- data.frame(id = 1:3, val_x = c("a", "b", "c"))
  df_y <- data.frame(id = 2:4, val_y = c("p", "q", "r"))

  # Test with join_type = "inner"
  fn_inner <- function(x, y, join_type = "inner") {
    if (join_type == "inner") dplyr::inner_join(x, y, by = "id")
    else if (join_type == "left") dplyr::left_join(x, y, by = "id")
    else dplyr::right_join(x, y, by = "id")
  }
  block_inner <- new_function_xy_block(fn = fn_inner)

  testServer(
    blockr.core::get_s3_method("block_server", block_inner),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 2L)
    },
    args = list(x = block_inner, data = list(x = function() df_x, y = function() df_y))
  )

  # Test with join_type = "left"
  fn_left <- function(x, y, join_type = "left") {
    if (join_type == "inner") dplyr::inner_join(x, y, by = "id")
    else if (join_type == "left") dplyr::left_join(x, y, by = "id")
    else dplyr::right_join(x, y, by = "id")
  }
  block_left <- new_function_xy_block(fn = fn_left)

  testServer(
    blockr.core::get_s3_method("block_server", block_left),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(x = block_left, data = list(x = function() df_x, y = function() df_y))
  )

  # Test with join_type = "right"
  fn_right <- function(x, y, join_type = "right") {
    if (join_type == "inner") dplyr::inner_join(x, y, by = "id")
    else if (join_type == "left") dplyr::left_join(x, y, by = "id")
    else dplyr::right_join(x, y, by = "id")
  }
  block_right <- new_function_xy_block(fn = fn_right)

  testServer(
    blockr.core::get_s3_method("block_server", block_right),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(x = block_right, data = list(x = function() df_x, y = function() df_y))
  )
})

test_that("function_xy_block combined parameter values are reflected in result", {
  # Test with n=2, all_rows=FALSE
  fn_combined <- function(x, y, n = 2L, all_rows = FALSE) {
    merged <- merge(x, y, by = "id", all = all_rows)
    utils::head(merged, n)
  }
  block <- new_function_xy_block(fn = fn_combined)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 2L)
    },
    args = list(x = block, data = list(x = function() df1, y = function() df2))
  )

  # Test with different values: n=1, all_rows=TRUE
  df_x <- data.frame(id = 1:3, val_x = c("a", "b", "c"))
  df_y <- data.frame(id = 2:4, val_y = c("p", "q", "r"))

  fn_combined2 <- function(x, y, n = 1L, all_rows = TRUE) {
    merged <- merge(x, y, by = "id", all = all_rows)
    utils::head(merged, n)
  }
  block2 <- new_function_xy_block(fn = fn_combined2)

  testServer(
    blockr.core::get_s3_method("block_server", block2),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 1L)
    },
    args = list(x = block2, data = list(x = function() df_x, y = function() df_y))
  )
})
