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
    '.lv <- unique(data$Species)',
    '.boom <- stop("should never run")',
    'x <- factor("setosa", .lv)',
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

test_that("a dotted name is scaffolding, whatever it holds", {
  script <- paste(
    '.keep <- c("Sepal.Length", "Nope")',
    '.keep <- intersect(.keep, names(data))',
    '',
    'col <- factor(.keep, levels = .keep)',
    '',
    'data[, as.character(col), drop = FALSE]',
    sep = "\n"
  )
  p <- cb_parse(script)
  specs <- cb_specs(p, iris)
  expect_equal(vapply(specs, `[[`, character(1L), "name"), "col")

  # `.keep <- f(.keep)` is ordinary R and reads the OLD `.keep`; without
  # carrying the previous binding in, the line reads itself and `col` dies of
  # infinite recursion.
  expect_null(specs[[1L]]$error)
  expect_equal(specs[[1L]]$choices, "Sepal.Length")

  # The editor paints exactly the lines that became controls.
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 4)
})

test_that("a dotted name is never a control, whatever it is assigned", {
  script <- paste(
    '.n <- 6',
    '.pick <- factor("setosa", unique(data$Species))',
    'label <- "total"',
    'utils::head(data, .n)',
    sep = "\n"
  )
  expect_equal(vapply(specs_for(script), `[[`, character(1L), "name"), "label")
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 3)

  # Still an ordinary variable: the body reads it and the line survives.
  e <- expr_for(script)
  expect_identical(e[[2L]], quote(.n <- 6))
  expect_equal(eval_bquoted(e, iris), utils::head(iris, 6))
})

test_that("a declaration below the header stays code, and says so", {
  script <- paste(
    'n <- 6',
    'out <- utils::head(data, n)',
    'label <- "total"',
    'out',
    sep = "\n"
  )
  p <- cb_parse(script)
  expect_equal(vapply(p$stmts, `[[`, logical(1L), "input"), c(TRUE, FALSE, FALSE, FALSE))
  expect_true(p$stmts[[3L]]$late)
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 1)
  expect_match(cb_rest_label(cb_specs(p, iris), p), "line 3")

  # It is still an ordinary assignment, so it has to survive as code.
  expect_identical(expr_for(script)[[3L]], quote(label <- "total"))
})

test_that("a pool call declares a multi-select over what the data has", {
  script <- paste(
    'vars <- intersect(names(data), c("Species", "Petal.Width", "NOPE"))',
    'data[, vars, drop = FALSE]',
    sep = "\n"
  )
  s <- specs_for(script)
  expect_length(s, 1L)
  expect_equal(s[[1L]]$kind, "select")
  expect_true(s[[1L]]$multiple)
  # intersect() keeps the order of its first argument, so the pool is in
  # column order rather than the order they were written down.
  expect_equal(s[[1L]]$choices, c("Petal.Width", "Species"))

  # The pool is the default, so an untouched control keeps every column.
  expect_equal(
    cb_expr(cb_parse(script), s, list()),
    bquote(.(data_slot())[, .(c("Petal.Width", "Species")), drop = FALSE])
  )
  expect_equal(
    eval_bquoted(expr_for(script, list(vars = "Species")), iris),
    iris[, "Species", drop = FALSE]
  )
})

test_that("a pool stays a multi-select when it matches one column", {
  # Multiplicity is fixed by the script, so a pool that narrows to one today
  # must not turn into a text box.
  s <- specs_for('vars <- intersect(names(data), c("Species", "NOPE"))\ndata[, vars]')
  expect_equal(s[[1L]]$kind, "select")
  expect_true(s[[1L]]$multiple)
})

test_that("a pool call that is not a vector is an ordinary line", {
  # `unique()` is in the pool list, but this one returns a data frame. The line
  # keeps its meaning, gets no control, and above all is not dropped from the
  # body: `nrow(dedup)` must still have a `dedup`.
  script <- "dedup <- unique(data)\nnrow(dedup)"
  expect_length(specs_for(script), 0L)
  e <- expr_for(script)
  expect_identical(e[[2L]], bquote(dedup <- unique(.(data_slot()))))
  expect_equal(eval_bquoted(e, iris), nrow(unique(iris)))
})

test_that("a dotted helper gets no control and no band", {
  script <- paste(
    '.lv <- unique(data$Species)',
    'pick <- factor("setosa", .lv)',
    'data[data$Species == pick, ]',
    sep = "\n"
  )
  s <- specs_for(script)
  expect_equal(vapply(s, `[[`, character(1L), "name"), "pick")
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 2)
  # It is the convention working, so the footer does not comment on it.
  expect_match(cb_rest_label(s, cb_parse(script)), "^1 input")
})

test_that("a name declared twice keeps only the last control", {
  script <- paste(
    'vars <- c("Species", "Petal.Width")',
    'vars <- intersect(vars, names(data))',
    'data[, vars, drop = FALSE]',
    sep = "\n"
  )
  p <- cb_parse(script)
  expect_equal(attr(cb_demoted(p), "reason"), c("redeclared", "", ""))

  s <- cb_specs(p, iris)
  expect_length(s, 1L)
  expect_equal(s[[1L]]$line, 2)
  expect_match(cb_rest_label(s, p), "vars declared twice")
  # The first line is not a control, so it has to survive as code.
  expect_identical(expr_for(script)[[2L]], quote(vars <- c("Species", "Petal.Width")))
})

test_that("an assignment target is not substituted, but its index is", {
  expect_identical(cb_subst(quote(n <- n + 1), list(n = 6)), quote(n <- 6 + 1))
  expect_identical(cb_subst(quote(x[n] <- n), list(n = 6)), quote(x[6] <- 6))
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
    paste0(".unused <- { ", sprintf(bump, "helper", "helper"), "; c('F', 'M') }"),
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
    ".lv <- { options(cb_helper = getOption('cb_helper') + 1L); c('a', 'b') }",
    "pick <- factor('a', levels = .lv)",
    "also <- factor('b', levels = .lv)",
    "nrow(data)",
    sep = "\n"
  )

  specs <- cb_specs(cb_parse(script), data.frame(x = 1))

  expect_identical(vapply(specs, `[[`, character(1L), "kind"),
                   c("select", "select"))
  # Read by two declarations, evaluated once: the active binding caches.
  expect_identical(getOption("cb_helper"), 1L)
})


# ---- scoping --------------------------------------------------------------

test_that("a formal shadows a control of the same name inside the function", {
  # `function(n)` binds `n` for its body, so the body's `n` is the argument,
  # not the knob. Substituting it would rewrite the lambda.
  e <- expr_for("n <- 3\nsapply(1:2, function(n) n + 1)")
  expect_identical(e, quote(sapply(1:2, function(n) n + 1)))

  # ... including through the backslash lambda, and in a default.
  expect_identical(
    expr_for("n <- 3\nsapply(1:2, \\(n) n + 1)"),
    quote(sapply(1:2, function(n) n + 1))
  )
  expect_identical(
    expr_for("n <- 3\nsapply(1:2, function(x, k = n) x + k)"),
    quote(sapply(1:2, function(x, k = 3) x + k))
  )

  # A formal that does NOT clash leaves the substitution alone.
  expect_identical(
    expr_for("n <- 3\nsapply(1:2, \\(x) x + n)"),
    quote(sapply(1:2, function(x) x + 3))
  )
})

test_that("a formal named data shadows the block's data slot", {
  # The bug this guards: every group returning nrow() of the whole table.
  e <- expr_for("vapply(split(data, data$cyl), function(data) nrow(data), 1L)")
  expect_identical(
    e,
    quote(vapply(split(.(data), .(data)$cyl), function(data) nrow(data), 1L))
  )
})

test_that("a loop variable demotes the declaration it collides with", {
  # Plain R leaves `n` at 2 after the loop, so the script cannot mean a knob.
  p <- cb_parse("n <- 3\nfor (n in 1:2) print(n)\nhead(data, n)")
  expect_identical(cb_shadowed(p), "n")

  e <- expr_for("n <- 3\nfor (n in 1:2) print(n)\nhead(data, n)")
  expect_identical(e[[2L]], quote(n <- 3))
  expect_identical(e[[3L]], quote(for (n in 1:2) print(n)))

  expect_match(
    cb_rest_label(specs_for("n <- 3\nfor (n in 1:2) print(n)\nhead(data, n)"), p),
    "n assigned in the body"
  )
})


# ---- construction ---------------------------------------------------------

test_that("the constructor rejects a script that cannot be used", {
  expect_error(new_code_block(script = "head(data,"), "Failed to parse script")
  expect_error(new_code_block(script = "n <- 6"), "no body")
  expect_s3_class(new_code_block(), "code_block")
})

test_that("the footer reports a parse failure of an external write", {
  expect_match(
    cb_rest_label(list(), cb_parse("head(data,")),
    "does not parse"
  )
})

test_that("an empty factor declares a multi-select with nothing picked", {
  # "Pick any number of these, starting from none" has no other spelling: the
  # declaration's length is the only signal, so zero cannot mean single.
  s <- specs_for('x <- factor(character(0), c("a", "b", "c"))')[[1L]]

  expect_identical(s$kind, "select")
  expect_true(s$multiple)
  expect_identical(s$choices, c("a", "b", "c"))
})


# ---- params grid ----------------------------------------------------------

test_that("the params grid clamps its column count in every container band", {
  # A band carries min(fields, cap) rather than a literal, so a two-field grid
  # is never widened into a row with an empty trailing track.
  expect_identical(fb_grid_cols(2), "--fb-cols:2; --fb-cols-md:2; --fb-cols-sm:2;")
  expect_identical(fb_grid_cols(3), "--fb-cols:3; --fb-cols-md:3; --fb-cols-sm:2;")
  expect_identical(fb_grid_cols(7), "--fb-cols:4; --fb-cols-md:3; --fb-cols-sm:2;")
})
