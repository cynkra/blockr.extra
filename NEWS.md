# blockr.extra (development version)

## Features

- **The generated params band lays its fields out by kind.** It was a grid of
  equal columns, which serves none of them: at a half-width dock panel four
  tracks give a select 154px, where one tag plus the overflow chip is all that
  fits, while the checkbox beside it wastes the same 154px. The band is now a
  flex row where each field asks for the width its kind needs and takes a share
  of what is left over, so a select gets 300px before anything wraps and a row
  of knobs still fits on one line. Measured over ten plausible scripts it holds
  one row wherever the grid did, and grows one only where a select needs the
  room.

- **A checkbox lines up with the fields either side of it.** It labels itself
  beside the box, so it had no label row and started 23px above its neighbours.
  It now gets a spacer row and the same 42px shell as every other control.

- **Long tag values are shortened from the middle** (`tag_chars`, 16 by
  default), so "Xanomeline High Dose" and "Xanomeline Low Dose" stay apart on
  the card instead of both reading "Xanomelin…". Full value on hover.

- **Every generated multi-select keeps its tags on one row**, with the overflow
  counted on a `+N` chip. Needs blockr.dplyr 0.2.0.9008.
