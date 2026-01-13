# Test function_var_block

testServer <- shiny::testServer

# Sample data for testing
df1 <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  id = 4:6,
  name = c("Diana", "Eve", "Frank"),
  stringsAsFactors = FALSE
)

df3 <- data.frame(
  id = 7:9,
  name = c("Grace", "Hank", "Ivy"),
  stringsAsFactors = FALSE
)

test_that("new_function_var_block creates a valid block", {
  block <- new_function_var_block()

  expect_s3_class(block, "function_var_block")
  expect_s3_class(block, "block")
})

test_that("new_function_var_block accepts custom functions", {
  my_fn <- function(...) {
    dplyr::bind_rows(...)
  }

  block <- new_function_var_block(fn = my_fn)

  expect_s3_class(block, "function_var_block")
})

test_that("function_var_block produces correct result with default function", {
  block <- new_function_var_block()  # Default is bind_rows(...)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 6L)  # 3 + 3 rows from bind_rows
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1,
          `2` = df2
        )
      )
    )
  )
})

test_that("function_var_block works with three inputs", {
  block <- new_function_var_block()

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 9L)  # 3 + 3 + 3 rows
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1,
          `2` = df2,
          `3` = df3
        )
      )
    )
  )
})

test_that("function_var_block with custom reduce function", {
  # Function that uses Reduce to merge all data frames
  reduce_fn <- function(...) {
    dfs <- list(...)
    if (length(dfs) == 1) return(dfs[[1]])
    Reduce(function(x, y) merge(x, y, all = TRUE), dfs)
  }

  block <- new_function_var_block(fn = reduce_fn)

  # Create test data with common column
  d1 <- data.frame(id = 1:2, a = c("x", "y"))
  d2 <- data.frame(id = 2:3, b = c("p", "q"))

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(is.data.frame(result))
      expect_true("a" %in% colnames(result))
      expect_true("b" %in% colnames(result))
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = d1,
          `2` = d2
        )
      )
    )
  )
})

test_that("function_var_block with parameters produces correct result", {
  bind_fn <- function(..., .id = NULL) {
    dplyr::bind_rows(..., .id = .id)
  }

  block <- new_function_var_block(fn = bind_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 6L)
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1,
          `2` = df2
        )
      )
    )
  )
})

test_that("function_var_block auto-detects GT output", {
  # Function that returns a gt object
  gt_fn <- function(...) {
    combined <- dplyr::bind_rows(...)
    gt::gt(combined)
  }

  block <- new_function_var_block(fn = gt_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_s3_class(result, "gt_tbl")
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1,
          `2` = df2
        )
      )
    )
  )
})

test_that("function_var_block can be reconstructed from function text", {
  original_fn <- function(...) {
    dplyr::bind_rows(...)
  }

  fn_text <- paste(deparse(original_fn), collapse = "\n")
  restored_fn <- eval(parse(text = fn_text))
  restored_block <- new_function_var_block(fn = restored_fn)

  testServer(
    blockr.core::get_s3_method("block_server", restored_block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 6L)
    },
    args = list(
      x = restored_block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1,
          `2` = df2
        )
      )
    )
  )
})

test_that("function_var_block handles single input", {
  block <- new_function_var_block()

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
      expect_equal(result, df1)
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1
        )
      )
    )
  )
})

test_that("function_var_block with named inputs", {
  block <- new_function_var_block()

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 6L)
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          first = df1,
          second = df2
        )
      )
    )
  )
})

test_that("function_var_block handles different parameter types", {
  typed_fn <- function(
    ...,
    n = 5L,                    # numeric
    add_source = TRUE          # logical
  ) {
    result <- dplyr::bind_rows(...)
    if (add_source) {
      result$.combined <- TRUE
    }
    utils::head(result, n)
  }

  block <- new_function_var_block(fn = typed_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(nrow(result), 5L)
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1,
          `2` = df2
        )
      )
    )
  )
})

test_that("function_var_block with summary function", {
  summary_fn <- function(...) {
    combined <- dplyr::bind_rows(...)
    data.frame(
      total_rows = nrow(combined),
      total_cols = ncol(combined),
      unique_ids = length(unique(combined$id))
    )
  }

  block <- new_function_var_block(fn = summary_fn)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_equal(result$total_rows, 9L)
      expect_equal(result$unique_ids, 9L)
    },
    args = list(
      x = block,
      data = list(
        ...args = shiny::reactiveValues(
          `1` = df1,
          `2` = df2,
          `3` = df3
        )
      )
    )
  )
})

# Tests verifying different parameter values are reflected in results

test_that("function_var_block numeric parameter values are reflected in result", {
  # Test with n = 3
  fn_3 <- function(..., n = 3L) {
    combined <- dplyr::bind_rows(...)
    utils::head(combined, n)
  }
  block_3 <- new_function_var_block(fn = fn_3)

  testServer(
    blockr.core::get_s3_method("block_server", block_3),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 3L)
    },
    args = list(
      x = block_3,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2, `3` = df3))
    )
  )

  # Test with n = 6
  fn_6 <- function(..., n = 6L) {
    combined <- dplyr::bind_rows(...)
    utils::head(combined, n)
  }
  block_6 <- new_function_var_block(fn = fn_6)

  testServer(
    blockr.core::get_s3_method("block_server", block_6),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 6L)
    },
    args = list(
      x = block_6,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2, `3` = df3))
    )
  )

  # Test with n = 9 (all rows)
  fn_9 <- function(..., n = 9L) {
    combined <- dplyr::bind_rows(...)
    utils::head(combined, n)
  }
  block_9 <- new_function_var_block(fn = fn_9)

  testServer(
    blockr.core::get_s3_method("block_server", block_9),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 9L)
    },
    args = list(
      x = block_9,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2, `3` = df3))
    )
  )
})

test_that("function_var_block logical parameter values are reflected in result", {
  # Test with add_source_id = FALSE
  fn_no_source <- function(..., add_source_id = FALSE) {
    if (add_source_id) dplyr::bind_rows(..., .id = "source")
    else dplyr::bind_rows(...)
  }
  block_no_source <- new_function_var_block(fn = fn_no_source)

  testServer(
    blockr.core::get_s3_method("block_server", block_no_source),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_false("source" %in% names(result))
    },
    args = list(
      x = block_no_source,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2))
    )
  )

  # Test with add_source_id = TRUE
  fn_with_source <- function(..., add_source_id = TRUE) {
    if (add_source_id) dplyr::bind_rows(..., .id = "source")
    else dplyr::bind_rows(...)
  }
  block_with_source <- new_function_var_block(fn = fn_with_source)

  testServer(
    blockr.core::get_s3_method("block_server", block_with_source),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true("source" %in% names(result))
    },
    args = list(
      x = block_with_source,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2))
    )
  )
})

test_that("function_var_block character parameter values are reflected in result", {
  # Test with prefix = "row"
  fn_row <- function(..., prefix = "row") {
    combined <- dplyr::bind_rows(...)
    combined$label <- paste0(prefix, "_", seq_len(nrow(combined)))
    utils::head(combined, 5)
  }
  block_row <- new_function_var_block(fn = fn_row)

  testServer(
    blockr.core::get_s3_method("block_server", block_row),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(grepl("^row_", result$label)))
    },
    args = list(
      x = block_row,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2))
    )
  )

  # Test with prefix = "entry"
  fn_entry <- function(..., prefix = "entry") {
    combined <- dplyr::bind_rows(...)
    combined$label <- paste0(prefix, "_", seq_len(nrow(combined)))
    utils::head(combined, 5)
  }
  block_entry <- new_function_var_block(fn = fn_entry)

  testServer(
    blockr.core::get_s3_method("block_server", block_entry),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_true(all(grepl("^entry_", result$label)))
    },
    args = list(
      x = block_entry,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2))
    )
  )
})

test_that("function_var_block select parameter values are reflected in result", {
  # Create test data with some overlap
  d1 <- data.frame(id = 1:3, name = c("A", "B", "C"))
  d2 <- data.frame(id = 2:4, name = c("B", "C", "D"))

  # Test with method = "bind"
  fn_bind <- function(..., method = "bind") {
    dfs <- list(...)
    if (method == "bind") dplyr::bind_rows(...)
    else if (method == "intersect") Reduce(dplyr::intersect, dfs)
    else Reduce(dplyr::union, dfs)
  }
  block_bind <- new_function_var_block(fn = fn_bind)

  testServer(
    blockr.core::get_s3_method("block_server", block_bind),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 6L)
    },
    args = list(
      x = block_bind,
      data = list(...args = shiny::reactiveValues(`1` = d1, `2` = d2))
    )
  )

  # Test with method = "intersect"
  fn_intersect <- function(..., method = "intersect") {
    dfs <- list(...)
    if (method == "bind") dplyr::bind_rows(...)
    else if (method == "intersect") Reduce(dplyr::intersect, dfs)
    else Reduce(dplyr::union, dfs)
  }
  block_intersect <- new_function_var_block(fn = fn_intersect)

  testServer(
    blockr.core::get_s3_method("block_server", block_intersect),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 2L)  # B and C
    },
    args = list(
      x = block_intersect,
      data = list(...args = shiny::reactiveValues(`1` = d1, `2` = d2))
    )
  )

  # Test with method = "union"
  fn_union <- function(..., method = "union") {
    dfs <- list(...)
    if (method == "bind") dplyr::bind_rows(...)
    else if (method == "intersect") Reduce(dplyr::intersect, dfs)
    else Reduce(dplyr::union, dfs)
  }
  block_union <- new_function_var_block(fn = fn_union)

  testServer(
    blockr.core::get_s3_method("block_server", block_union),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 4L)  # A, B, C, D
    },
    args = list(
      x = block_union,
      data = list(...args = shiny::reactiveValues(`1` = d1, `2` = d2))
    )
  )
})

test_that("function_var_block combined parameter values are reflected in result", {
  # Test with n=4, add_source=TRUE
  fn_combined <- function(..., n = 4L, add_source = TRUE) {
    if (add_source) combined <- dplyr::bind_rows(..., .id = "source")
    else combined <- dplyr::bind_rows(...)
    utils::head(combined, n)
  }
  block <- new_function_var_block(fn = fn_combined)

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 4L)
      expect_true("source" %in% names(result))
    },
    args = list(
      x = block,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2, `3` = df3))
    )
  )

  # Test with different values: n=2, add_source=FALSE
  fn_combined2 <- function(..., n = 2L, add_source = FALSE) {
    if (add_source) combined <- dplyr::bind_rows(..., .id = "source")
    else combined <- dplyr::bind_rows(...)
    utils::head(combined, n)
  }
  block2 <- new_function_var_block(fn = fn_combined2)

  testServer(
    blockr.core::get_s3_method("block_server", block2),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(nrow(result), 2L)
      expect_false("source" %in% names(result))
    },
    args = list(
      x = block2,
      data = list(...args = shiny::reactiveValues(`1` = df1, `2` = df2, `3` = df3))
    )
  )
})
