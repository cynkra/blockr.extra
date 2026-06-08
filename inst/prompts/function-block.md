Write the value of `fn` as an R function `function(data, ...) { ... }`. `data` is
the block's input (the upstream result); the function returns the block's output.

## The function-block contract

- **First argument is `data`** — the input. Reference only columns/elements that
  actually exist; explore first (`names(data)`, `str(data)`,
  `sort(unique(data$col))`).
- **Every extra argument MUST have a default.** A parameter without a default
  crashes the block. The default's *type* decides the UI control it generates
  (see the table below).
- **The return value sets the output type**: a `data.frame`/tibble → interactive
  table; a `ggplot` → plot; a `gt` table → HTML table; a base plot → image; any
  other object → its printed text.
- **R style**: use the base pipe `|>` (never `%>%`); namespace-prefix every
  non-base/stats call (`dplyr::filter()`, `stringr::str_detect()`).
- **Serialization**: `fn` is embedded as a JSON string, so escape newlines.
  Multi-line bodies are fine; for a short function, a single line with `;`
  between statements is also fine.

## Parameters → interactive UI controls

Each argument after `data` becomes a control in the block's gear panel, and **its
default value decides the control type**. Whatever the user selects is passed
into your function under that name.

| Default you write | Control rendered | Pre-selected | Your arg receives |
|---|---|---|---|
| `x = list("A" = "a", "B" = "b")` | **multi-select** | **all** choices | character **vector** of chosen values |
| `x = c("A" = "a", "B" = "b")` (named) | single-select dropdown; names are labels | first only | length-1 character (the value, e.g. `"a"`) |
| `x = c("a", "b", "c")` | single-select dropdown | first only | length-1 character |
| `x = 6L` / `x = 0.95` | numeric input | — | length-1 numeric |
| `x = TRUE` | checkbox | — | length-1 logical |
| `x = "USUBJID"` | text input | — | length-1 character |
| `x = NULL` (or no default) | text input (empty) | — | `""` until the user types |

**The rule to remember: `list(...)` → multi-select with everything pre-selected;
`c(...)` → single-select with the first pre-selected.** For both, prefer a
*named* vector/list — the names are the on-screen labels, the values are what
your code receives. For column- or value-pickers, populate the choices with the
ACTUAL values from the data (`sort(unique(data$col))`), not just the few visible
in the preview.

Example:

    function(data,
             column = c("Sepal.Length" = "Sepal.Length", "Sepal.Width" = "Sepal.Width"),
             n = 6L,
             descending = FALSE) {
      data <- data[order(data[[column]], decreasing = descending), ]
      utils::head(data, n)
    }
