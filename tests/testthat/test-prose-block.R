test_that("new_prose_block constructs a text block", {
  b <- new_prose_block("hello")
  expect_s3_class(b, "prose_block")
  expect_s3_class(b, "text_block")
})

test_that("prose block evaluates glue against the named data input", {
  b <- new_prose_block(
    "Rows: {nrow(data)}; mean mpg {round(mean(data$mpg), 1)}"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        as.character(session$returned$result()),
        "Rows: 32; mean mpg 20.1"
      )
    },
    args = list(
      x = b,
      data = list(...args = shiny::reactiveValues(data = mtcars))
    )
  )
})

test_that("plain markdown (no glue) passes through", {
  b <- new_prose_block("## Title\n\nSome **bold** text.")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        as.character(session$returned$result()),
        "## Title\n\nSome **bold** text."
      )
    },
    args = list(
      x = b,
      data = list(...args = shiny::reactiveValues(data = mtcars))
    )
  )
})

test_that("text state is a reactiveVal (external_ctrl contract)", {
  b <- new_prose_block("init")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      expect_true(inherits(session$returned$state$text, "reactiveVal"))
      expect_identical(session$returned$state$text(), "init")
    },
    args = list(...args = shiny::reactiveValues(data = mtcars))
  )
})

test_that("UI edits flow into state (text input -> reactiveVal)", {
  b <- new_prose_block("init")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      session$setInputs(text = "## Edited\n\n{nrow(data)} rows")
      session$flushReact()
      expect_identical(
        session$returned$state$text(),
        "## Edited\n\n{nrow(data)} rows"
      )
    },
    args = list(...args = shiny::reactiveValues(data = mtcars))
  )
})

test_that("external (AI) writes reach state, and re-render through result()", {
  b <- new_prose_block("init")

  # External-control write into the state reactiveVal updates it.
  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      session$returned$state$text("Total rows: {nrow(data)}")
      session$flushReact()
      expect_identical(
        session$returned$state$text(),
        "Total rows: {nrow(data)}"
      )
    },
    args = list(...args = shiny::reactiveValues(data = mtcars))
  )

  # A block restored with that markdown renders it (round-trip via ctor).
  restored <- new_prose_block("Total rows: {nrow(data)}")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", restored),
    {
      session$flushReact()
      expect_identical(
        as.character(session$returned$result()),
        "Total rows: 32"
      )
    },
    args = list(
      x = restored,
      data = list(...args = shiny::reactiveValues(data = mtcars))
    )
  )
})
