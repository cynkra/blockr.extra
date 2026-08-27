# Test code_block: the script-with-declared-inputs block.

testServer <- shiny::testServer

specs_for <- function(script, data = iris) {
  cb_specs(cb_parse(script), data)
}

expr_for <- function(script, values = list(), data = iris) {
  p <- cb_parse(script)
  cb_expr(p, cb_specs(p, data), values)
}


# ---- classification -------------------------------------------------------

test_that("a plain-value assignment is an input, anything else is code", {
  p <- cb_parse(paste(
    'n <- 6',
    'label <- "total"',
    'desc <- TRUE',
    'when <- as.Date("2026-01-01")',
    'pick <- factor("a", c("a", "b"))',
    'keep <- c("a", "b")',
    'neg <- -1',
    'tmp <- data |> dplyr::filter(TRUE)',
    'half <- nrow(data) / 2',
    'twice <- 6L * 2',
    'f <- function(x) x + 1',
    'utils::head(tmp)',
    sep = "\n"
  ))

  expect_true(p$ok)
  is_input <- vapply(p$stmts, `[[`, logical(1L), "input")
  names(is_input) <- vapply(p$stmts, `[[`, character(1L), "name")

  expect_equal(
    names(is_input)[is_input],
    c("n", "label", "desc", "when", "pick", "keep", "neg")
  )
  expect_equal(sum(!is_input), 5L)
})

test_that("input lines carry their source line number", {
  p <- cb_parse("n <- 6\n\nlabel <- \"x\"\n\nutils::head(data, n)")
  lines <- vapply(p$stmts, `[[`, integer(1L), "line")
  expect_equal(lines, c(1L, 3L, 5L))
})

test_that("a parse error is reported, not thrown", {
  p <- cb_parse("data |> dplyr::filter(")
  expect_false(p$ok)
  expect_type(p$error, "character")
})


# ---- widgets --------------------------------------------------------------

test_that("a factor is a select whose levels are the choices", {
  s <- specs_for('x <- factor("setosa", unique(data$Species))')[[1L]]
  expect_equal(s$kind, "select")
  expect_equal(s$choices, c("setosa", "versicolor", "virginica"))
  expect_false(s$multiple)
  expect_equal(s$default, "setosa")
})

test_that("multiplicity comes from the declaration's length", {
  s <- specs_for('x <- factor(c("setosa", "virginica"), unique(data$Species))')[[1L]]
  expect_true(s$multiple)
  expect_equal(s$default, c("setosa", "virginica"))
  # ... and stays multiple when the live value narrows to one pick, or the
  # control would change type under the user.
  expect_true(specs_for('x <- factor(c("a", "b"), c("a", "b", "c"))')[[1L]]$multiple)
})

test_that("a factor also carries a fixed option list, no data needed", {
  s <- specs_for('m <- factor("pearson", c("pearson", "spearman"))')[[1L]]
  expect_equal(s$kind, "select")
  expect_equal(s$choices, c("pearson", "spearman"))
})

test_that("literals map to their widgets", {
  kinds <- vapply(
    specs_for('n <- 6\nlabel <- "total"\ndesc <- TRUE\nd <- as.Date("2026-01-01")'),
    `[[`, character(1L), "kind"
  )
  expect_equal(kinds, c("number", "text", "flag", "date"))
})

test_that("a bare vector is the value, so the pool is that vector", {
  s <- specs_for('keep <- c("a", "b")')[[1L]]
  expect_equal(s$kind, "select")
  expect_true(s$multiple)
  expect_equal(s$choices, c("a", "b"))
  expect_equal(s$default, c("a", "b"))
})

test_that("a declaration can read a helper line above it, lazily", {
  # The pipeline in between must NOT be evaluated to get there.
  script <- paste(
    'lv <- unique(data$Species)',
    'boom <- stop("should never run")',
    'x <- factor("setosa", lv)',
    sep = "\n"
  )
  s <- specs_for(script)
  expect_length(s, 1L)
  expect_equal(s[[1L]]$choices, c("setosa", "versicolor", "virginica"))
})

test_that("a declaration that errors becomes a reported spec, not a crash", {
  s <- specs_for('x <- factor("a", stop("boom"))')[[1L]]
  expect_true(is.na(s$kind))
  expect_type(s$error, "character")

  # A mistyped column yields a factor with no levels, which would otherwise be
  # an empty dropdown hiding the mistake.
  empty <- specs_for('x <- factor("a", unique(data$nope))')[[1L]]
  expect_true(is.na(empty$kind))
  expect_match(empty$error, "no choices")
})


# ---- annotations ----------------------------------------------------------

test_that("the #| annotation carries what a value cannot say", {
  expect_equal(cb_annotation('n <- 6  #| number(min = 1, max = 50)'),
               list(min = 1, max = 50))
  expect_equal(cb_annotation('n <- 6  #| min = 1, max = 50'),
               list(min = 1, max = 50))
  expect_equal(cb_annotation('n <- 6  #| label = "Rows"'), list(label = "Rows"))
  expect_equal(cb_annotation('n <- 6'), list())
  expect_equal(cb_annotation('n <- 6  #| nonsense ='), list())
})

test_that("annotation values reach the spec", {
  s <- specs_for('n <- 6   #| number(min = 1, max = 50, label = "Rows")')[[1L]]
  expect_equal(s$min, 1)
  expect_equal(s$max, 50)
  expect_equal(s$label, "Rows")
})


# ---- the expression -------------------------------------------------------

test_that("values are spliced in as literals and the data slot is used", {
  e <- expr_for(
    'species <- factor("setosa", unique(data$Species))\n\ndplyr::filter(data, Species == species)'
  )
  expect_equal(e, bquote(dplyr::filter(.(data_slot()), Species == "setosa")))
  expect_false(has_bare_data(e))
})

test_that("the expression carries no assignment, so no local() wrapper", {
  # This is the whole point: blockr.code wraps in local() when it finds an
  # assignment, which is what made the function block's export unreadable.
  e <- expr_for('n <- 6\n\nutils::head(data, n)')
  expect_equal(e, bquote(utils::head(.(data_slot()), 6)))
  expect_false(any(all.names(e) %in% c("<-", "=", "function")))
})

test_that("live values override the declaration's defaults", {
  script <- 'species <- factor("setosa", unique(data$Species))\nn <- 6\n\ndplyr::slice_head(dplyr::filter(data, Species %in% species), n = n)'
  e <- expr_for(script, values = list(species = c("setosa", "virginica"), n = 3))
  expect_equal(
    e,
    bquote(dplyr::slice_head(
      dplyr::filter(.(data_slot()), Species %in% .(c("setosa", "virginica"))),
      n = 3
    ))
  )
})

test_that("the expression evaluates to the same thing the script means", {
  e <- expr_for('n <- 3\n\nutils::head(data, n)')
  expect_equal(eval_bquoted(e, iris), utils::head(iris, 3))
})

test_that("a checkbox folds its branch away", {
  script <- paste(
    'desc <- TRUE',
    '',
    'dplyr::arrange(data, if (desc) dplyr::desc(Sepal.Length) else Sepal.Length)',
    sep = "\n"
  )
  expect_equal(
    expr_for(script, values = list(desc = TRUE)),
    bquote(dplyr::arrange(.(data_slot()), dplyr::desc(Sepal.Length)))
  )
  expect_equal(
    expr_for(script, values = list(desc = FALSE)),
    bquote(dplyr::arrange(.(data_slot()), Sepal.Length))
  )
})

test_that("an else-less branch is dropped from a + chain, not left as NULL", {
  script <- paste(
    'trend <- TRUE',
    '',
    'ggplot2::ggplot(data) + ggplot2::geom_point() + if (trend) ggplot2::geom_smooth()',
    sep = "\n"
  )
  on_expr <- expr_for(script, values = list(trend = TRUE))
  expect_true("geom_smooth" %in% all.names(on_expr))

  off_expr <- expr_for(script, values = list(trend = FALSE))
  expect_false("geom_smooth" %in% all.names(off_expr))
  expect_false("if" %in% all.names(off_expr))
  expect_equal(
    off_expr,
    bquote(ggplot2::ggplot(.(data_slot())) + ggplot2::geom_point())
  )
})

test_that("a Date is emitted as as.Date(), not as a structure() blob", {
  e <- expr_for('cutoff <- as.Date("2026-01-01")\n\ndplyr::filter(data, when > cutoff)')
  expect_equal(
    e,
    bquote(dplyr::filter(.(data_slot()), when > as.Date("2026-01-01")))
  )
  expect_false("structure" %in% all.names(e))
})

test_that("only value positions are substituted", {
  # `n` as an argument NAME and as the head of dplyr::n() must survive; only
  # the value reference is replaced.
  e <- expr_for('n <- 6\n\ndplyr::summarise(data, n = dplyr::n(), k = n)')
  expect_equal(
    e,
    bquote(dplyr::summarise(.(data_slot()), n = dplyr::n(), k = 6))
  )
})

test_that("a name the body assigns stays code and offers no control", {
  script <- 'n <- 6\n\nn <- nrow(data)\nutils::head(data, n)'
  p <- cb_parse(script)
  expect_equal(cb_shadowed(p), "n")

  e <- cb_expr(p, cb_specs(p, iris), list())
  # The declaration is kept as a statement, so the script still means what it
  # says; nothing is silently rewritten.
  expect_true("<-" %in% all.names(e))
  expect_equal(eval_bquoted(e, iris), utils::head(iris, nrow(iris)))
})

test_that("multiple body statements are wrapped in a single braced expression", {
  e <- expr_for('n <- 2\n\ntmp <- utils::head(data, n)\nnrow(tmp)')
  expect_identical(e[[1L]], quote(`{`))
  expect_equal(eval_bquoted(e, iris), 2L)
})

test_that("a script with no body yields no expression", {
  expect_null(expr_for('n <- 6'))
})


# ---- coercion -------------------------------------------------------------

test_that("values coming back from widgets are coerced by the declaration", {
  num <- specs_for('n <- 6')[[1L]]
  expect_equal(cb_coerce("12", num), 12)
  expect_null(cb_coerce("NA", num))

  flag <- specs_for('x <- TRUE')[[1L]]
  expect_identical(cb_coerce("FALSE", flag), FALSE)

  sel <- specs_for('x <- factor(c("a", "b"), c("a", "b", "c"))')[[1L]]
  # A JS array arrives as a list.
  expect_equal(cb_coerce(list("a", "c"), sel), c("a", "c"))
})

test_that("a restored value survives the trip back into the expression", {
  script <- 'species <- factor("setosa", unique(data$Species))\n\ndplyr::filter(data, Species == species)'
  # A saved board hands the value back as a list (JSON array of one).
  e <- expr_for(script, values = list(species = list("virginica")))
  expect_equal(
    e,
    bquote(dplyr::filter(.(data_slot()), Species == "virginica"))
  )
})


# ---- editor marks ---------------------------------------------------------

test_that("marks are derived without evaluating anything", {
  marks <- cb_syntactic_marks(
    'x <- factor("a", unique(data$nope))\nn <- 6\ntmp <- data |> utils::head()'
  )
  expect_length(marks, 2L)
  expect_equal(vapply(marks, `[[`, integer(1L), "line"), c(1L, 2L))
  expect_equal(vapply(marks, `[[`, character(1L), "kind"), c("select", "number"))
})

test_that("marks are empty while the script does not parse", {
  expect_length(cb_syntactic_marks("data |> dplyr::filter("), 0L)
})


# ---- the block ------------------------------------------------------------

test_that("new_code_block creates a valid block", {
  blk <- new_code_block()
  expect_s3_class(blk, "code_block")
  expect_s3_class(blk, "block")
  expect_identical(attr(blk, "expr_type"), "bquoted")
})

test_that("the block's expression and state come out of the script", {
  script <- 'n <- 3\n\nutils::head(data, n)'
  block <- new_code_block(script = script)
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 3))
      )
      expect_equal(session$returned$state$script(), script)
    },
    args = list(x = block, data = list(data = function() datasets::iris))
  )
})

test_that("knob positions are part of the state, so a board restores them", {
  script <- 'n <- 3\n\nutils::head(data, n)'
  block <- new_code_block(script = script, values = list(n = 7))
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(session$returned$state$values(), list(n = 7))
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 7))
      )
    },
    args = list(x = block, data = list(data = function() datasets::iris))
  )
})

test_that("an external write of the script re-derives the expression", {
  # The path blockr.ai's external_ctrl takes. `session$setInputs` does not
  # reach the block's inner module namespace in testServer, so drive the state
  # reactive directly -- which is what an external write does anyway.
  block <- new_code_block(script = 'n <- 3\n\nutils::head(data, n)')
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 3))
      )

      session$returned$state$script('n <- 5\n\nutils::head(data, n)')
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 5))
      )

      # A declaration that disappears takes its stored value with it.
      session$returned$state$script('utils::head(data, 2)')
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 2))
      )
    },
    args = list(x = block, data = list(data = function() datasets::iris))
  )
})

test_that("the block evaluates end to end through blockr", {
  block <- new_code_block(
    script = 'species <- factor("versicolor", unique(data$Species))\n\ndplyr::filter(data, Species == species)'
  )
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "data.frame")
      expect_true(all(as.character(result$Species) == "versicolor"))
      expect_equal(nrow(result), 50L)
    },
    args = list(x = block, data = list(data = function() datasets::iris))
  )
})

test_that("the footer says how many controls the script produced", {
  p <- cb_parse('n <- 6\n\nutils::head(data, n)')
  expect_match(cb_rest_label(cb_specs(p, iris), p), "^1 input ")

  p0 <- cb_parse('utils::head(data)')
  expect_match(cb_rest_label(cb_specs(p0, iris), p0), "^no inputs")
})

# ---- hiding a panel must not disturb the expression ------------------------

test_that("a declaration that reads the data survives the data going away", {
  # The visibility gate makes `data()` THROW while a block's panel is hidden,
  # not merely go stale. Recomputing the specs against that absence produced an
  # error spec, cb_expr() dropped the declaration as unusable, and the emitted
  # expression silently lost its substitution -- referring to a symbol nothing
  # defines. Coming back restored the good expression, so the block re-evaluated
  # on every tab switch.
  script <- paste(
    'pick <- factor(data$Species[1], levels(data$Species))',
    '',
    'subset(data, Species == pick)',
    sep = "\n"
  )
  block <- new_code_block(script = script)

  on_screen <- shiny::reactiveVal(TRUE)
  data_slot <- function() {
    if (!on_screen()) {
      shiny::req(FALSE)     # what the gate does to a hidden block
    }
    datasets::iris
  }

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      shown <- session$returned$expr()
      expect_true(is.call(shown))
      # the declaration is inlined as a literal, not left as a bare symbol
      expect_false(any(grepl("pick", deparse(shown), fixed = TRUE)))
      expect_true(any(grepl("setosa", deparse(shown), fixed = TRUE)))

      on_screen(FALSE)      # panel hidden: data() now throws
      session$flushReact()
      expect_equal(session$returned$expr(), shown)

      on_screen(TRUE)       # and shown again
      session$flushReact()
      expect_equal(session$returned$expr(), shown)
    },
    args = list(x = block, data = list(data = data_slot))
  )
})
test_that("specs are kept, not degraded, when the data is unavailable", {
  # The unit the fix turns on: cb_specs() against NULL yields an error spec, so
  # the block must hold the last good one instead of adopting it.
  script <- 'pick <- factor(data$Species[1], levels(data$Species))\n\nsubset(data, Species == pick)'
  p <- cb_parse(script)

  good <- cb_specs(p, datasets::iris)
  degraded <- cb_specs(p, NULL)

  expect_length(good, 1L)
  expect_null(good[[1L]]$error)
  expect_false(is.na(good[[1L]]$kind))

  # this is what the block must never adopt
  expect_length(degraded, 1L)
  expect_false(is.null(degraded[[1L]]$error))

  # and what it costs if it does: the substitution disappears
  expect_true(any(grepl("setosa", deparse(cb_expr(p, good, list())), fixed = TRUE)))
  expect_true(any(grepl("pick", deparse(cb_expr(p, degraded, list())), fixed = TRUE)))
})

test_that("building the controls runs the header, never the body", {
  # The point of `data` being in scope for a declaration is that a select's
  # choices can come from the data (`factor("F", unique(data$SEX))`). Paying
  # for that with a second run of the block's pipeline would be a bad trade:
  # the pipeline is the expensive part, and it already runs once as the
  # block's expr.
  #
  # Counted through options() rather than a helper function on purpose:
  # cb_specs() evaluates in blockr.core::eval_env(), whose parent is
  # baseenv(), so nothing from the test's own scope would be reachable.
  on.exit(options(cb_body = NULL, cb_helper = NULL), add = TRUE)
  options(cb_body = 0L, cb_helper = 0L)
  bump <- "options(cb_%s = getOption('cb_%s') + 1L)"

  script <- paste(
    "sex <- factor('F', levels = sort(unique(data$SEX)))",
    paste0("unused <- { ", sprintf(bump, "helper", "helper"), "; c('F', 'M') }"),
    paste0("out <- { ", sprintf(bump, "body", "body"), "; 42 }"),
    "out",
    sep = "\n"
  )

  specs <- cb_specs(
    cb_parse(script),
    data.frame(SEX = c("F", "M", "F"), stringsAsFactors = FALSE)
  )

  # The declaration was evaluated, and its choices came off the data.
  expect_length(specs, 1L)
  expect_identical(specs[[1L]]$kind, "select")
  expect_identical(specs[[1L]]$choices, c("F", "M"))

  # Nothing else was. The body never ran, and the helper no declaration reads
  # stayed unforced behind its lazy binding (cb_delay).
  expect_identical(getOption("cb_body"), 0L)
  expect_identical(getOption("cb_helper"), 0L)
})

test_that("a helper a declaration actually reads is forced, once", {
  on.exit(options(cb_helper = NULL), add = TRUE)
  options(cb_helper = 0L)

  script <- paste(
    "lv <- { options(cb_helper = getOption('cb_helper') + 1L); c('a', 'b') }",
    "pick <- factor('a', levels = lv)",
    "also <- factor('b', levels = lv)",
    "nrow(data)",
    sep = "\n"
  )

  specs <- cb_specs(cb_parse(script), data.frame(x = 1))

  expect_identical(vapply(specs, `[[`, character(1L), "kind"),
                   c("select", "select"))
  # Read by two declarations, evaluated once: the active binding caches.
  expect_identical(getOption("cb_helper"), 1L)
})
