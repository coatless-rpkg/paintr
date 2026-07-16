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

- **A list column of a data frame says what is in it.** Every cell of a list
  column used to read `<list>` -- the same six characters whatever it held, so
  five different elements looked identical and the reader learned nothing. It now
  reads `<int [3]>`, `<chr [1]>`, `<dbl [2 x 2]>`, one description per element,
  which is what `print()` on a tibble has done for years. The type row still says
  `<list>`, because the column genuinely is one.

## New features

- **`paint_list()` and `gpaint_list()` draw a list.** The elements are the
  columns and the values inside an element are the rows, so a list is drawn as
  the picture a data frame already had -- with the shared length taken away.
  **A data frame IS a list whose elements happen to share a length**, and
  `paint_list(list(a = 1:3, b = 4:6))` next to
  `paint_data_frame(data.frame(a = 1:3, b = 4:6))` is that sentence as a picture.
  Take the shared length away and the rectangle breaks; nothing else about the
  drawing changes.

  A named element is labelled `$a` and an unnamed one `[[2]]`, exactly as
  `print()` labels them. `show_indices = "cell"` draws `[[j]][i]` under each
  value -- the accessor students get wrong most often, printed under the number
  it returns. `summarise = TRUE` draws each element as one cell saying what it
  is, which is what a forty-element list wants.

  **The picture does not draw nesting.** A sublist is a grey `<list [2]>` and
  stops there, as is any element that is not a plain vector: a matrix element
  draws as `<int [2 x 2]>`, a data frame element as `<df [5 x 3]>`. Elision is
  asked of each element separately, so a short element never draws a `...` for
  values it does not have -- which means a drawn row of a long list can hold
  `a[9]` beside `c[7]`, and is one more reason there is no `[i]` gutter.

  **The heavy outline appears exactly when the elements share a length** -- which
  is exactly when the list could have been a data frame. A ragged block is not a
  rectangle, and a box around it would enclose cells that do not exist, so it
  gets none. The rectangle closing is the lesson, not decoration on it.

  A list carrying a class -- `as.POSIXlt(Sys.time())`, an `lm`, a `t.test()`
  result -- is refused rather than drawn: `is.list()` is `TRUE` for all of them,
  and a datetime drawn as eleven ragged columns of `sec`, `min`, `hour`, ... is a
  wrong picture, not a picture of a list. `highlight_data()` gained a `list`
  method that accepts exactly what the painter accepts.

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
