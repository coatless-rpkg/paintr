# paintr 0.0.1

Initial CRAN submission. The package was developed under the name `drawr`; the
notes below describe what changed for anyone who used it under that name.

## Breaking changes

Two defaults changed, and both are visible in the picture:

- **Numbers are drawn at 3 significant figures.** They used to be drawn at
  whatever `as.character()` produced, which for a value like `1/3` was fifteen
  digits overflowing the cell. Pass `sigfig` to ask for more or fewer.

- **Large structures elide their middle.** A matrix or vector past 20 rows or
  15 columns -- a data frame past 10 rows or 10 columns -- now has its middle
  replaced by a row of `...`, with a `# 130 more rows` note under the drawing.
  Pass `max_rows`, `max_cols`, or `show_all = TRUE` to draw every cell.

## New features

- **Names are drawn.** A named vector labels its cells with its `names()`, a
  matrix with `dimnames()` labels its rows and columns with them, and a data
  frame with row names of its own -- `mtcars`, not `iris` -- grows a row-name
  gutter. The picture used to draw `[1]` and `[, 1]` over data that had names,
  which made it *less* informative than `print()`. Turn the lanes off with
  `show_names`, `show_dimnames = "none"` or `show_rownames = FALSE`.

  An index lane you ask for wins the axis it names, so `show_indices = "row"`
  still draws `[1, ]`. To see a name **and** an accessor, ask for
  `show_indices = "cell"`: the cell index is drawn inside the cell, under the
  value, so it composes with the names rather than competing for their lane.

  A matrix is one formatting unit, so its longest column name widens every cell
  in the picture; column names are truncated at `max_name_chars` (8) to bound the
  cost. Row names sit in a gutter that holds no value, cost the cells nothing,
  and are truncated at `max_chars` (12) like any other string.

- **Data frames are supported.** `paint_data_frame()` and `gpaint_data_frame()`
  (aliases: `paint_df()`, `gpaint_df()`) draw a data frame with its column names
  and a `<dbl>` / `<chr>` / `<lgl>` type band beneath them (`show_names`,
  `show_types`). Each column is formatted on its own, so a huge value in one
  column will not flip another column into scientific notation.

- **`gpaint_vector()`**, completing the base/ggplot2 pairing: every structure
  paintr draws now has both a `paint_*()` and a `gpaint_*()`.

- **`paint_size()`** returns the device size a structure needs in order to stay
  legible, as `c(width = , height = )` in inches, centimetres, or pixels. Paste
  it into a `png()` call or a knitr chunk header. It opens no device and reads no
  device, so it answers the question *before* you have a device that is too
  small.

- **Cell text is fitted to the space available** instead of being drawn at a
  fixed size, so cells no longer overflow. Pass `fontsize` to override the fit;
  `family` chooses the font.

- **Digits past the last significant one are greyed, not discarded.**
  `123456.789` is drawn as a black `123` followed by a grey `457.`, so the
  magnitude of a number is never hidden by rounding it -- only de-emphasized.
  `subtle_digits` chooses which digits go grey, or turns the effect off.

- Long strings are truncated to `max_chars` with an ellipsis, instead of running
  out of the cell.

## Bug fixes

- **Character values render as themselves.** `paint_matrix()` and
  `paint_vector()` previously drew a red `Unknown` in every cell of a character
  matrix or vector: the type test they used recognized only finite numbers,
  `Inf`, `NaN` and `NA`, and a string fell off the end of it.

- **`highlight_data()` no longer errors on character vectors, logical vectors, or
  data frames**, and neither do `highlight_rows()`, `highlight_columns()` or
  `highlight_locations()`. Rows and columns of a data frame may be named as well
  as numbered.

- **Graphics parameters are restored after drawing.** `paint_matrix()` and
  `paint_vector()` used to leave `par(mar = )` set to paintr's margins, so the
  caller's next plot inherited them.

- `paint_matrix()`'s type error no longer claims the data should be a `vector`.
