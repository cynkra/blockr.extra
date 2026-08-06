# The rail geometry is JavaScript, so its tests are too (tests/js/). Running
# them from testthat as well means `devtools::test()` and CI see a red light
# when the layout breaks its invariants, instead of the JS suite quietly
# rotting next to a package nobody thinks to `npm test`.
test_that("the rail geometry keeps its invariants (node --test)", {

  skip_on_cran()

  node <- Sys.which("node")
  skip_if(!nzchar(node), "node is not installed")

  js_dir <- testthat::test_path("..", "js")
  skip_if(!dir.exists(js_dir), "tests/js is not present")

  files <- list.files(js_dir, pattern = "[.]test[.]js$", full.names = TRUE)
  skip_if(!length(files), "no JS tests found")

  out <- suppressWarnings(
    system2(node, c("--test", files), stdout = TRUE, stderr = TRUE)
  )

  status <- attr(out, "status")

  expect_true(
    is.null(status) || identical(status, 0L),
    info = paste(out, collapse = "\n")
  )
})
