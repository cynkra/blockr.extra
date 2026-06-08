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
- **Formatting**: write **readable, multi-line code with indentation** — it is
  shown to the user in a code editor. Put each statement (and each function
  argument, when there are several) on its own line; do NOT cram the body onto a
  single line with `;`. Real newlines are fine and expected.

## Parameters → interactive UI controls

**Adding a function argument is THE way to give the user a control.** When the
request is to "expose", "let the user choose/pick/select/change", "make X
selectable", "add a dropdown/filter/option for X", you do it by adding an
argument after `data` -- NOT by looking for some library- or domain-specific
selector/filter API. There is no other mechanism: a control exists if and only
if there is a function parameter for it.

Each argument after `data` becomes a control in the block's gear panel, and **its
default value decides the control type**. Whatever the user selects is passed
into your function under that name, and you use it in the body (e.g.
`dplyr::filter(data, Species %in% chosen)` or a composer `levels = chosen`).

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

Example (note `sort_by = c(...)` is a single-select, `keep = list(...)` is a
multi-select):

    function(data,
             sort_by = c("Sepal.Length" = "Sepal.Length", "Sepal.Width" = "Sepal.Width"),
             keep = list("Sepal.Length" = "Sepal.Length", "Species" = "Species"),
             n = 6L) {
      data <- data[order(data[[sort_by]]), unlist(keep), drop = FALSE]
      utils::head(data, n)
    }
