# blockr.extra

Experimental function blocks for [blockr](https://github.com/cynkra/blockr).

## Installation

```r
pak::pak("cynkra/blockr.extra")
```

## Blocks

- `new_function_block(fn)` - Transform data with a custom function (first arg: `data`)
- `new_function_xy_block(fn)` - Transform two data frames (first args: `x`, `y`)
- `new_function_var_block(fn)` - Transform multiple data frames (first arg: `...`)

UI is auto-generated from function argument defaults. Output auto-detects GT tables vs DataTables.
