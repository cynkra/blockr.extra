# is_html_renderable(): the generic renderer's HTML branch gate. It must accept
# objects that carry HTML via the htmltools::as.tags() contract (gt, htmlwidgets,
# raw tags) and reject plain data so it reaches the DataTable / text renderers.

test_that("is_html_renderable() accepts HTML-carrying objects", {
  ihr <- is_html_renderable
  skip_if_not_installed("gt")
  expect_true(ihr(gt::gt(utils::head(mtcars, 2))))          # registered as.tags method
  expect_true(ihr(htmltools::HTML("<b>x</b>")))             # native html
  expect_true(ihr(htmltools::div("x")))                     # native tag
  skip_if_not_installed("DT")
  expect_true(ihr(DT::datatable(utils::head(mtcars, 2))))   # htmlwidget
})

test_that("is_html_renderable() rejects plain data (so it reaches other renderers)", {
  ihr <- is_html_renderable
  expect_false(ihr("a plain string"))
  expect_false(ihr(42))
  expect_false(ihr(mtcars))                                 # data.frame -> DataTable branch
  expect_false(ihr(ggplot2::ggplot(mtcars)))               # ggplot -> renderPlot branch
  expect_false(ihr(structure(list(1, 2), class = "not_html")))
})
