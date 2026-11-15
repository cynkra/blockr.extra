# blockr.extra (development version)

## Deprecated

- **The code block and the function blocks are deprecated.** Use the Script
  block, `blockr.script::new_script_block()`, instead. `new_code_block()`,
  `new_function_block()`, `new_function_var_block()`, `new_function_xy_block()`
  and `new_async_function_block()` stay so that saved boards keep working.
  Each shows a line above its result saying it is deprecated, writes a warning
  to the R log once per session, and is listed as "(deprecated)" in the block
  registry, where the assistant is told to add the Script block instead.

## Features

- **The generated params band drops a column instead of squeezing a select.**
  It keeps its equal tracks, so the fields stay aligned down the band and
  across its rows; what changed is where the column count steps down. The old
  ladder was built for a 150px track, right for a number or a flag and far too
  tight for a select, so a half-width panel kept four columns and gave each
  select 154px, where the overflow chip was all that fitted: four tidy boxes
  saying nothing about what was selected. A band holding a select or a text now
  steps at 220px, and adds a third and fourth column only once every column
  would still be 330px. Measured on a nine-field band: 1768px is four columns
  of 430, 703px two of 346, 453px two of 220.

- **A checkbox lines up with the fields either side of it.** It labels itself
  beside the box, so it had no label row and started 23px above its neighbours.
  It now gets a spacer row and the same 42px shell as every other control.

- **Long tag values are shortened from the middle** (`tag_chars`, 16 by
  default), so "Xanomeline High Dose" and "Xanomeline Low Dose" stay apart on
  the card instead of both reading "Xanomelin…". Full value on hover.

- **Every generated multi-select keeps its tags on one row**, with the overflow
  counted on a `+N` chip. Needs blockr.dplyr 0.2.0.9008.
