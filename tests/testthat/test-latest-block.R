# Test new_latest_block — FRP merge / latest-wins semantics
#
# We test the inner server function directly (via block[["expr_server"]])
# rather than going through blockr.core::block_server.block, because the
# full dispatch path nests moduleServer inside moduleServer and Shiny's
# testServer doesn't re-fire per-key reactiveValues observers reliably
# across that nesting. The block works correctly in a real Shiny runtime;
# the quirk is limited to the test harness.

testServer <- shiny::testServer

df1 <- data.frame(src = "one",   x = 1)
df2 <- data.frame(src = "two",   x = 2)
df3 <- data.frame(src = "three", x = 3)

test_that("new_latest_block creates a valid variadic block", {
  block <- new_latest_block()

  expect_s3_class(block, "latest_block")
  expect_s3_class(block, "transform_block")
  expect_true(is.na(blockr.core::block_arity(block)))
})

test_that("latest_block falls back to first input on mount", {
  # With ignoreInit = TRUE the per-input observers don't treat the
  # topo-sort mount reads as "clicks", so nothing is marked active by
  # them — the fallback branch picks the first input name.
  block <- new_latest_block()
  rv <- shiny::reactiveValues(a = df1, b = df2)

  testServer(
    block[["expr_server"]],
    {
      session$flushReact()
      expect_equal(session$returned$state$last_active(), "a")
    },
    args = list(`...args` = rv)
  )
})

test_that("latest_block switches to whichever input most recently fired", {
  block <- new_latest_block()
  rv <- shiny::reactiveValues(a = df1, b = df2)

  testServer(
    block[["expr_server"]],
    {
      session$flushReact()
      expect_equal(session$returned$state$last_active(), "a")  # fallback

      rv$b <- data.frame(src = "two-updated", x = 22)
      session$flushReact()
      expect_equal(session$returned$state$last_active(), "b")

      rv$a <- data.frame(src = "one-updated", x = 11)
      session$flushReact()
      expect_equal(session$returned$state$last_active(), "a")
    },
    args = list(`...args` = rv)
  )
})

test_that("latest_block honours last_active on construction", {
  block <- new_latest_block(last_active = "b")
  rv <- shiny::reactiveValues(a = df1, b = df2)

  testServer(
    block[["expr_server"]],
    {
      session$flushReact()
      expect_equal(session$returned$state$last_active(), "b")
    },
    args = list(`...args` = rv)
  )
})

test_that("latest_block handles three inputs", {
  block <- new_latest_block()
  rv <- shiny::reactiveValues(a = df1, b = df2, c = df3)

  testServer(
    block[["expr_server"]],
    {
      session$flushReact()
      expect_equal(session$returned$state$last_active(), "a")  # fallback

      rv$c <- data.frame(src = "three-updated", x = 33)
      session$flushReact()
      expect_equal(session$returned$state$last_active(), "c")
    },
    args = list(`...args` = rv)
  )
})
