# Package index

## Painters

Draw an R data structure as a picture, one function per structure. Each
page documents both backends: a base-graphics painter (`paint_*()`,
drawn to the current device) and a ggplot2 painter (`gpaint_*()`,
returning a ggplot object). Under every cell the painter writes the
expression you would type to reach that value.

- [`paint_vector()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-vector.md)
  [`gpaint_vector()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-vector.md)
  : Visualize Data Inside of a Vector
- [`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md)
  [`gpaint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md)
  : Visualize Data Inside of a Matrix
- [`paint_data_frame()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
  [`gpaint_data_frame()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
  [`paint_df()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
  [`gpaint_df()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
  : Visualize Data Inside of a Data Frame
- [`paint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md)
  [`gpaint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md)
  : Visualize Data Inside of a List
- [`paint_array()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-array.md)
  [`gpaint_array()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-array.md)
  : Visualize Data Inside of an Array

## Highlighting

Mark specific cells for a painter to fill.

- [`highlight_data()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md)
  [`highlight_rows()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md)
  [`highlight_columns()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md)
  [`highlight_locations()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/highlight-data.md)
  : Highlight data

## Sizing

Estimate the device size a structure needs so its text stays above the
legibility floor.

- [`paint_size()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint_size.md)
  : The device size a data structure needs
