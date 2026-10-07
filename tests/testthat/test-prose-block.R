test_that("new_prose_block constructs a text block", {
  b <- new_prose_block("hello")
  expect_s3_class(b, "prose_block")
  expect_s3_class(b, "text_block")
})

test_that("prose block evaluates inline R against the named data input", {
  b <- new_prose_block(
    "Rows: `r nrow(data)`; mean mpg `r round(mean(data$mpg), 1)`"
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

test_that("plain markdown (no inline R) passes through", {
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
      session$setInputs(text = "## Edited\n\n`r nrow(data)` rows")
      session$flushReact()
      expect_identical(
        session$returned$state$text(),
        "## Edited\n\n`r nrow(data)` rows"
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
      session$returned$state$text("Total rows: `r nrow(data)`")
      session$flushReact()
      expect_identical(
        session$returned$state$text(),
        "Total rows: `r nrow(data)`"
      )
    },
    args = list(...args = shiny::reactiveValues(data = mtcars))
  )

  # A block restored with that markdown renders it (round-trip via ctor).
  restored <- new_prose_block("Total rows: `r nrow(data)`")
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

# Capture custom messages: assign over the root MockShinySession method
# (the blockr.dplyr test-ready-handshake.R pattern).
capture_messages <- function(session) {
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  root <- session$rootScope()
  root$sendCustomMessage <- function(type, message) {
    sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
    invisible(NULL)
  }
  sent
}

last_of <- function(sent, type) {
  hits <- Filter(function(m) identical(m$type, type), sent$msgs)
  if (length(hits)) hits[[length(hits)]]$message else NULL
}

test_that("inline expressions evaluate individually; one failure is local", {
  b <- new_prose_block("x")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      sent <- capture_messages(session)
      session$setInputs(
        prose_exprs = list("nrow(data)", "mean(no_such_object)")
      )
      session$flushReact()

      vals <- last_of(sent, "prose-values")$values
      expect_true(vals[["nrow(data)"]]$ok)
      expect_identical(vals[["nrow(data)"]]$value, "32")
      expect_false(vals[["mean(no_such_object)"]]$ok)
      expect_match(vals[["mean(no_such_object)"]]$value, ".+")
    },
    args = list(...args = shiny::reactiveValues(data = mtcars))
  )
})

test_that("the value preview is dormant (ok = NA) before data arrives", {
  b <- new_prose_block("x")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      sent <- capture_messages(session)
      session$setInputs(prose_exprs = list("nrow(data)"))
      session$flushReact()

      vals <- last_of(sent, "prose-values")$values
      expect_true(is.na(vals[["nrow(data)"]]$ok))
    },
    args = list(...args = shiny::reactiveValues(data = NULL))
  )
})

test_that("evaluation keeps indentation and newlines", {
  b <- new_prose_block("- a\n  - b\n\nRows: `r nrow(data)`\n")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        paste(as.character(session$returned$result()), collapse = "\n"),
        "- a\n  - b\n\nRows: 32\n"
      )
    },
    args = list(
      x = b,
      data = list(...args = shiny::reactiveValues(data = mtcars))
    )
  )
})

test_that("braces are plain text", {
  b <- new_prose_block("::: {.callout-note}\nhi\n:::")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        paste(as.character(session$returned$result()), collapse = "\n"),
        "::: {.callout-note}\nhi\n:::"
      )
    },
    args = list(
      x = b,
      data = list(...args = shiny::reactiveValues(data = mtcars))
    )
  )
})

test_that("inline_r writes values in and leaves code alone", {
  d <- list(data = mtcars)
  expect_identical(inline_r("n = `r nrow(data)`", d), "n = 32")
  expect_identical(inline_r("`r 1 + 1` and `code`", d), "2 and `code`")
  expect_identical(
    inline_r("```r\n`r nrow(data)`\n```\n`r 2 * 2`", d),
    "```r\n`r nrow(data)`\n```\n4"
  )
  expect_identical(inline_r("`r letters[1:3]`"), "a, b, c")
  expect_error(inline_r("`r no_such_thing`"), "no_such_thing")
})

test_that("a prose block needs no input", {
  b <- new_prose_block("Just text.")
  expect_identical(blockr.core:::block_min_args(b), 0L)
})
