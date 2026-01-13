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
