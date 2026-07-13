test_that("annotated mode claims the .variable leaf", {

  drilled <- data.frame(
    .variable = c("SEX", "SEX"),
    .variable_level = c("F", "F"),
    n = c(53L, 90L)
  )

  cols <- drill_claim_columns(drilled, table = "adsl")

  expect_length(cols, 1L)
  expect_equal(cols[[1L]]$name, "SEX")
  expect_equal(cols[[1L]]$values, "F")
  expect_equal(cols[[1L]]$table, "adsl")
  expect_equal(cols[[1L]]$mode, "multi")
})

test_that("annotated mode claims enclosing groups, outermost first", {

  drilled <- data.frame(
    .group1 = "SOC", .group1_level = "Cardiac disorders",
    .variable = "PT", .variable_level = "Atrial fibrillation"
  )

  cols <- drill_claim_columns(drilled, table = "adae")

  expect_equal(vapply(cols, `[[`, "", "name"), c("SOC", "PT"))
  expect_equal(vapply(cols, `[[`, "", "values"),
               c("Cardiac disorders", "Atrial fibrillation"))
})

test_that("a dimension resolving to many values is not a claim", {

  # A header-row drill: the SOC is decided, its PT leaf is not.
  drilled <- data.frame(
    .group1 = "SOC", .group1_level = "Cardiac disorders",
    .variable = "PT", .variable_level = c("Atrial fibrillation", "Tachycardia")
  )

  cols <- drill_claim_columns(drilled, table = "adae")

  expect_length(cols, 1L)
  expect_equal(cols[[1L]]$name, "SOC")
})

test_that("an undrilled table claims nothing", {

  undrilled <- data.frame(
    .variable = c("SEX", "SEX"),
    .variable_level = c("F", "M"),
    n = c(143L, 111L)
  )

  expect_length(drill_claim_columns(undrilled, table = "adsl"), 0L)
})

test_that("source-column mode claims a chart drill (no ARD identity)", {

  # A chart strips the ARD columns (as_plain_df), so its drilled output is a
  # plain row subset -- nothing in it names the drilled column.
  drilled <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1023"),
    SEX = c("F", "F"),
    ARM = c("Placebo", "Placebo")
  )

  cols <- drill_claim_columns(drilled, table = "adsl", columns = c("SEX", "ARM"))

  expect_equal(vapply(cols, `[[`, "", "name"), c("SEX", "ARM"))
  expect_equal(vapply(cols, `[[`, "", "values"), c("F", "Placebo"))
})

test_that("source-column mode: an undrilled chart claims nothing", {

  undrilled <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1023"),
    SEX = c("F", "M")
  )

  expect_length(
    drill_claim_columns(undrilled, table = "adsl", columns = "SEX"),
    0L
  )
})

test_that("source-column mode ignores columns absent from the data", {

  drilled <- data.frame(SEX = c("F", "F"))

  cols <- drill_claim_columns(drilled, table = "adsl", columns = c("SEX", "NOPE"))

  expect_length(cols, 1L)
  expect_equal(cols[[1L]]$name, "SEX")
})

test_that("non-annotated data without `columns` claims nothing", {

  # The chart case, misconfigured: no ARD identity and no columns named.
  # Pass through silently rather than over-claiming coincidentally constant
  # columns (STUDYID, say).
  expect_length(
    drill_claim_columns(data.frame(STUDYID = "CDISCPILOT01", SEX = "F"),
                        table = "adsl"),
    0L
  )
  expect_length(drill_claim_columns(NULL, table = "adsl"), 0L)
  expect_length(drill_claim_columns("not a data frame", table = "adsl"), 0L)
})
