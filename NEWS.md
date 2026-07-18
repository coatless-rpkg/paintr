# paintr 0.0.1

Initial CRAN submission. The package was previously developed as `drawr`.

## Breaking changes

- Numbers draw at three significant figures by default, tunable with `sigfig`.
- Large matrices, vectors, and data frames elide their middle past a size cap,
  overridable with `max_rows`, `max_cols`, or `show_all = TRUE`.
- A data frame's list column shows each element's type and size, like
  `<int [3]>`, instead of a bare `<list>`.

## New features

- `paint_list()` and `gpaint_list()` draw a list, each element a column and its
  values the rows, which is the picture a data frame makes once its columns share
  a length. `show_indices = "cell"` labels cells with the `[[j]][i]` accessor, and
  `summarise = TRUE` collapses each element to a single cell.
- Named vectors, matrix `dimnames()`, and data-frame row names are drawn as
  labels, toggled with `show_names`, `show_dimnames`, and `show_rownames`.
- `paint_data_frame()` and `gpaint_data_frame()` (aliases `paint_df()` and
  `gpaint_df()`) draw a data frame with a per-column type band, controlled by
  `show_names` and `show_types`.
- `gpaint_vector()` completes the `paint_*()` and `gpaint_*()` pairing for every
  structure.
- `paint_size()` returns the device size a structure needs to stay legible, in
  inches, centimetres, or pixels.
- Cell text is fitted to the available space so it no longer overflows, with
  `fontsize` and `family` to override.
- Insignificant trailing digits are greyed rather than dropped, tunable with
  `subtle_digits`.
- Long strings are truncated to `max_chars` with an ellipsis.

## Bug fixes

- Character matrices and vectors render their values instead of a red `Unknown`.
- `highlight_data()` and the `highlight_rows()`, `highlight_columns()`, and
  `highlight_locations()` helpers no longer error on character vectors, logical
  vectors, or data frames, and accept row and column names.
- Graphics parameters are restored after `paint_matrix()` and `paint_vector()`.
- `paint_matrix()`'s type error no longer claims the data should be a `vector`.
