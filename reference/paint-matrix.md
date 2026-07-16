# Visualize Data Inside of a Matrix

Generate a graph showing the contents of a matrix.

## Usage

``` r
paint_matrix(
  data,
  show_indices = "none",
  highlight_area = NULL,
  highlight_color = "lemonchiffon",
  graph_title = paste0("Data Object: ", deparse(substitute(data))),
  graph_subtitle = NULL,
  sigfig = 3L,
  subtle_digits = c("insignificant", "rounded", "none"),
  max_chars = 12L,
  max_rows = 20L,
  max_cols = 15L,
  show_all = FALSE,
  fontsize = NULL,
  family = "mono",
  palette = NULL,
  show_dimnames = "all",
  max_name_chars = 8L,
  highlight_rows = NULL,
  highlight_columns = NULL,
  highlight_locations = NULL
)

gpaint_matrix(
  data,
  show_indices = "none",
  highlight_area = NULL,
  highlight_color = "lemonchiffon",
  graph_title = paste0("Data Object: ", deparse(substitute(data))),
  graph_subtitle = NULL,
  sigfig = 3L,
  subtle_digits = c("insignificant", "rounded", "none"),
  max_chars = 12L,
  max_rows = 20L,
  max_cols = 15L,
  show_all = FALSE,
  fontsize = NULL,
  family = "mono",
  palette = NULL,
  show_dimnames = "all",
  max_name_chars = 8L,
  highlight_rows = NULL,
  highlight_columns = NULL,
  highlight_locations = NULL
)
```

## Arguments

- data:

  An object that has the class of `matrix`.

- show_indices:

  Display indices based on location. A character vector, so several
  kinds of index can be asked for at once. Values are: `"none"`: no
  indices, `"cell"`: matrix cell indices `[i, j]`, `"row"`: row indices
  `[i, ]` to the left of the matrix, `"column"`: column indices `[, j]`
  above the matrix, and `"all"`: row, column, and cell indices together.
  Default: `"none"`.

  Each value switches on its own lane, so `c("row", "column")` draws the
  row *and* the column indices but no cell indices, and `"all"` is the
  same as `c("cell", "row", "column")`. Combining `"none"` with anything
  else is contradictory, and the other values win: `c("none", "row")`
  draws row indices. An unknown value is an error, not a silent no-op.

- highlight_area:

  Logical matrix the same shape as `data`, marking the cells to fill. A
  length-one logical is recycled. Default: `NULL`, which highlights
  nothing.

- highlight_color:

  Color to use to fill the background of a cell.

- graph_title:

  Title to appear in the upper left hand corner of the graph.

- graph_subtitle:

  Subtitle to appear immediately under the graph title. `NULL` (the
  default) describes the data: its dimensions and its class. `NA` or
  `""` draws no subtitle; any other string is drawn as given. The
  default reports the dimensions of the data itself, not of the drawing,
  so an elided matrix still reports all of its rows.

- sigfig:

  Significant digits drawn in black. Digits past the `sigfig`-th are
  drawn in grey; nothing is discarded. Must be in `1:15`.

- subtle_digits:

  Which digits are drawn grey. `"insignificant"` (the default) greys
  everything past the `sigfig`-th significant digit; `"rounded"` greys
  only digits the rendering actually lost; `"none"` draws everything
  black.

- max_chars:

  Strings longer than this are truncated with an ellipsis.

- max_rows, max_cols:

  Elide the middle of the matrix when it has more rows or columns than
  this. The decision is made on the dimensions alone, with no device
  consulted, so the same object elides the same way on every device.

- show_all:

  Draw every cell, however small the text becomes. Warns when the text
  falls below the legibility floor.

- fontsize:

  Font size in points. `NULL` (the default) fits the text to the device.

- family:

  Font family. `"mono"` by default.

- palette:

  Colour palette for the drawing. One of `"mint"` (the default),
  `"slate"`, `"warm"`, or `"classic"` (the original look). Defaults to
  the `"paintr.palette"` option when unset. A named list of colours is
  also accepted.

- show_dimnames:

  Which of the matrix's
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html) to draw. A
  character vector, because
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html) is a *list* with
  one slot per axis: `"none"`, `"row"`, `"column"`, or `"all"` (the
  default). `c("row", "column")` is the same as `"all"`, and an axis
  with no names draws no lane.

  An index lane the caller asks for **wins** the axis it names: with
  `show_indices = "row"` the row lane draws `[1, ]`, not the row names.
  To see both a name and an accessor, ask for `show_indices = "cell"` –
  the cell index is drawn *inside* the cell, so it composes with the
  names rather than competing for their lane.

- max_name_chars:

  Longest a *column* name may be drawn before it is truncated. Default:
  `8`. A matrix is one formatting unit, so every column of it is as wide
  as the widest: a long column name widens every cell in the picture.
  Row names sit in a gutter of their own, cost the values nothing, and
  are truncated at `max_chars` instead.

- highlight_rows, highlight_columns, highlight_locations:

  Shorthand for `highlight_area`: instead of building a mask, name the
  rows, columns, or cell locations to fill and the mask is built for you
  with
  [`highlight_data()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md).
  So `highlight_rows = 1` is exactly
  `highlight_area = highlight_rows(data, 1)`, and the object need not be
  named twice. Give several at once to fill their union. Supplying
  `highlight_area` together with any of these is an error. Default:
  `NULL`.

## Value

`paint_matrix()` invisibly returns the resolved cell table: a list with
the components `cells`, `fontsize`, `floored`, `u`, `x0`, `y0`, `usr`,
`pin`, `graph_title`, `graph_subtitle`, and – only when the drawing
elides – `note`. `graph_title` and `graph_subtitle` are the chrome as it
was actually drawn, so `graph_subtitle` is the resolved default rather
than the `NULL` that was passed in. `note` holds the "# N more
rows/columns" string and is present only when the drawing elides; it is
absent otherwise. `gpaint_matrix()` returns a `ggplot` object.

## Details

`paint_matrix()` draws on the current base graphics device.
`gpaint_matrix()` returns a `ggplot` object.

## The ggplot object is a shell

`gpaint_matrix()` returns a real `ggplot` object – `+ theme()`,
`ggsave()`, [`print()`](https://rdrr.io/r/base/print.html) and knitr
chunks all work – but its panel is drawn *entirely* by a custom grid
grob, held in a single `annotation_custom()` over a meaningless `0..1`
coordinate system. There is no `aes()`, no geom and no scale carrying
any meaning, so:

- `ggplot_build()` sees an empty layer.

- `+ scale_fill_*()`, `+ scale_x_*()` and friends have no effect on the
  drawing. Use `highlight_area` and `highlight_color` to fill cells.

- `+ geom_point()` would draw onto the `0..1` coordinate system, not
  onto the cells.

This is not a shortcut around ggplot2. The cell text is fitted to the
device at *draw* time, which no geom can do, because a layer is built
long before the device size is known; and every number is drawn as two
spans in two colors, which `geom_text()` cannot do at all. A custom grob
is the only mechanism that can do either.

## See also

Other painters:
[`paint_array()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-array.md),
[`paint_data_frame()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md),
[`paint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md),
[`paint_size()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint_size.md),
[`paint_vector()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-vector.md)

## Examples

``` r
# Base graphics

# Visualize a 3x3
mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
paint_matrix(mat_3x3)


# Show the cell indices
paint_matrix(mat_3x3, show_indices = "cell")


# Character data renders as the strings it contains.
paint_matrix(matrix(letters[1:6], nrow = 2))


# A dimnamed matrix labels its rows and columns with its names, like print().
mat_named = matrix(
  c(21, 6, 22.8, 4), nrow = 2, byrow = TRUE,
  dimnames = list(c("Mazda", "Datsun"), c("mpg", "cyl"))
)
paint_matrix(mat_named)


# The name above the column, the accessor under the value: both at once.
paint_matrix(mat_named, show_indices = "cell")


# An index lane wins the axis it names.
paint_matrix(mat_named, show_indices = "row")


# Or draw no names at all.
paint_matrix(mat_named, show_dimnames = "none")


# Highlight a row
mat_4x4 = matrix(seq_len(16), nrow = 4)
paint_matrix(
  mat_4x4, show_indices = "row",
  highlight_area = highlight_rows(mat_4x4, rows = 1)
)


# Highlight values above 5
mat_2x4 = matrix(round(rnorm(16, 5, 2), 2), ncol = 4)
paint_matrix(mat_2x4, highlight_area = mat_2x4 > 2)

# ggplot2 graphics ----

# Visualize a 3x3
mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
gpaint_matrix(mat_3x3)


# View the matrix without any highlighting
gpaint_matrix(mat_3x3, highlight_area = FALSE)


# Highlight a row
mat_2x2 = matrix(c(1, 2, 3, 4), nrow = 2)
mat_2x2_mask = matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2)
gpaint_matrix(mat_2x2, highlight_area = mat_2x2_mask)


# Highlight values above 5
mat_3x5 = matrix(round(rnorm(15, 5, 2), 2), ncol = 5)
gpaint_matrix(mat_3x5, highlight_area = mat_3x5 > 2)
```
