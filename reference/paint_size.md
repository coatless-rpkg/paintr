# The device size a data structure needs

How big the graphics device has to be for
[`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md)
and friends to draw `data` at the legibility floor. Opens no device and
reads no device, so it works in a fresh session with nothing plotted –
which is the whole point, since the situation it exists for is "the
device I have is too small".

## Usage

``` r
paint_size(
  data,
  ...,
  min_pt = 5,
  family = "mono",
  units = c("in", "cm", "px"),
  dpi = 96
)
```

## Arguments

- data:

  A vector, matrix, or data frame.

- ...:

  Any shape-affecting painter argument – for example `show_indices`,
  `summarise`, `show_all`, `layout`, `max_rows`, `max_cols`,
  `max_slices`, `sigfig`, `max_chars`, `show_names`, `show_types`,
  `show_dimnames`, `max_name_chars`, `name_align`, `type_align`, or a
  list's `gap`. These are passed to the cell builder, so the estimate
  matches what the painter would draw.

- min_pt:

  The legibility floor, in points. The returned size is the one that
  puts the fitted text exactly here.

- family:

  Font family. Only its metrics matter, and `measure_mono()` is used for
  all of them, so this argument currently changes nothing; it is present
  so the signature matches the painters'.

- units:

  `"in"`, `"cm"`, or `"px"`.

- dpi:

  Pixels per inch, used only when `units = "px"`.

## Value

A named numeric vector, `c(width = , height = )`. Inches and centimetres
are rounded up to a tenth; pixels are rounded up to a whole pixel.

## Details

The answer is a lower bound: at exactly this size the text lands on
`min_pt`, so round up in practice.

## See also

Other painters:
[`paint_array()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-array.md),
[`paint_data_frame()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md),
[`paint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md),
[`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md),
[`paint_vector()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-vector.md)

## Examples

``` r
# How large a device does a 20x20 matrix need, drawn in full?
paint_size(matrix(1:400, nrow = 20), show_all = TRUE)
#>  width height 
#>    4.7    2.7 

# The same question in pixels, for a png() at 96 dpi.
paint_size(iris, show_all = TRUE, units = "px")
#>  width height 
#>    423   1880 

# The answer is in inches by default, so it can be pasted straight into a
# device call or a knitr chunk header (fig.width, fig.height).
s <- paint_size(iris, show_all = TRUE)
s
#>  width height 
#>    4.4   19.6 
# png("iris.png", width = s[["width"]], height = s[["height"]],
#     units = "in", res = 96)
```
