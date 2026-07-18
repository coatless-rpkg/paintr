# Changelog

## paintr 0.0.1

Initial CRAN submission. The package was previously developed as
`drawr`.

### Breaking changes

- Numbers draw at three significant figures by default, tunable with
  `sigfig`.
- Large matrices, vectors, and data frames elide their middle past a
  size cap, overridable with `max_rows`, `max_cols`, or
  `show_all = TRUE`.
- A data frame’s list column shows each element’s type and size, like
  `<int [3]>`, instead of a bare `<list>`.

### New features

- [`paint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md)
  and
  [`gpaint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md)
  draw a list, each element a column and its values the rows, which is
  the picture a data frame makes once its columns share a length.
  `show_indices = "cell"` labels cells with the `[[j]][i]` accessor, and
  `summarise = TRUE` collapses each element to a single cell.
- Named vectors, matrix
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html), and data-frame
  row names are drawn as labels, toggled with `show_names`,
  `show_dimnames`, and `show_rownames`.
- [`paint_data_frame()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
  and
  [`gpaint_data_frame()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
  (aliases
  [`paint_df()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
  and
  [`gpaint_df()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md))
  draw a data frame with a per-column type band, controlled by
  `show_names` and `show_types`.
- [`gpaint_vector()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-vector.md)
  completes the `paint_*()` and `gpaint_*()` pairing for every
  structure.
- [`paint_size()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint_size.md)
  returns the device size a structure needs to stay legible, in inches,
  centimetres, or pixels.
- Cell text is fitted to the available space so it no longer overflows,
  with `fontsize` and `family` to override.
- Insignificant trailing digits are greyed rather than dropped, tunable
  with `subtle_digits`.
- Long strings are truncated to `max_chars` with an ellipsis.

### Bug fixes

- Character matrices and vectors render their values instead of a red
  `Unknown`.
- [`highlight_data()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md)
  and the
  [`highlight_rows()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md),
  [`highlight_columns()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md),
  and
  [`highlight_locations()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md)
  helpers no longer error on character vectors, logical vectors, or data
  frames, and accept row and column names.
- Graphics parameters are restored after
  [`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md)
  and
  [`paint_vector()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-vector.md).
- [`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md)’s
  type error no longer claims the data should be a `vector`.
