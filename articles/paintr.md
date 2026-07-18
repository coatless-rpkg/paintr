# Getting started with paintr

`paintr` draws R’s data structures as teaching diagrams. This guide
tours the five structures it can draw, the two backends it draws them
with, and the arguments that shape each picture. Load the package to
follow along.

``` r

library(paintr)
```

## What paintr draws

When you teach R, the hard part is rarely the arithmetic. It is helping
someone *see* the shape of the thing they are working with, so they
picture a vector as a row of boxes, a data frame as a stack of columns
that happen to line up, and an array as a pile of matrices. paintr draws
exactly those pictures.

``` r

paint_vector(c(4, 8, 15, 16, 23, 42))
```

![A numeric vector of six values, 4, 8, 15, 16, 23, and 42, drawn as a
column of cells; under each cell is its positional accessor, \[1\]
through \[6\], the expression you type to reach that
value.](paintr_files/figure-html/unnamed-chunk-3-1.png)

Every picture is the same idea. Each value sits in its own cell, and
under each cell paintr writes **the expression you would type to reach
it**. The label under the third box above is `[3]`, because `x[3]` is
how you would pull that value out. That is the whole teaching trick: the
drawing and the code are the same object.

## Two backends: `paint_` and `gpaint_`

Every structure has two painters. The `paint_*()` functions draw in base
graphics, straight to the device. The `gpaint_*()` functions return a
**ggplot object**, so you can print it, save it with `ggsave()`, or drop
it into a patchwork.

``` r

gpaint_vector(c(4, 8, 15, 16, 23, 42))
```

![The same six values, 4, 8, 15, 16, 23, and 42, drawn as a column of
cells with the accessors \[1\] through \[6\] beneath them, this time
rendered as a ggplot object rather than base graphics; the picture is
identical.](paintr_files/figure-html/unnamed-chunk-4-1.png)

The two draw the same picture, with one difference in the ggplot
version. Its panel is a single custom grob, not a layer of geoms. That
is what lets paintr defer text fitting to draw time and paint the grey
trailing digits you will see below, but it also means `ggplot_build()`
sees an empty layer, and adding `+ scale_fill_*()` has no effect. Reach
for `gpaint_*()` when you want a ggplot in your hand, and `paint_*()`
otherwise. The rest of this tour uses `paint_*()`.

## Vectors

A vector is a one-dimensional run of values, and that is how it draws.

``` r

paint_vector(c(-3, 5, NA, Inf, 2, 1))
```

![A numeric vector of six values drawn as a column of cells holding -3,
5, NA, Inf, 2, and 1; the missing value shows as NA and the infinite
value as Inf, and each cell carries its positional accessor \[1\]
through \[6\].](paintr_files/figure-html/unnamed-chunk-5-1.png)

Two options earn their keep here. `layout = "horizontal"` turns the
column on its side, and `show_indices` writes the position labels
`[1] [2] ...` either `"inside"` each cell or `"outside"` the row.

``` r

paint_vector(1:6, layout = "horizontal", show_indices = "outside")
```

![The numbers 1 through 6 drawn as a single horizontal row of cells
instead of a column, with the positional labels \[1\] through \[6\]
printed outside the row beneath each
cell.](paintr_files/figure-html/unnamed-chunk-6-1.png)

Give the vector names and it draws each name as the accessor you would
type, `["mon"]` rather than a bare `mon`, because `x["mon"]` is how you
reach the value. The through-line holds: the label under every cell is
still exactly the expression you type. Set `show_names = FALSE` to fall
back to `[1] [2]`.

``` r

paint_vector(c(mon = 12, tue = 19, wed = 3, thu = 8))
```

![A named numeric vector of four values, 12, 19, 3, and 8, drawn as a
column of cells; each cell is labelled with its name as a character
accessor, \["mon"\], \["tue"\], \["wed"\], and \["thu"\], the expression
you type to reach that
value.](paintr_files/figure-html/unnamed-chunk-7-1.png)

## Matrices

A matrix is a grid, so it draws as one.
[`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md)
also handles a 2-D array or a 2-D table.

``` r

paint_matrix(matrix(1:15, nrow = 3))
```

![The numbers 1 through 15 drawn as a 3 by 5 grid of cells filled column
by column, so the first column reads 1, 2, 3 down and the last reads 13,
14, 15.](paintr_files/figure-html/unnamed-chunk-8-1.png)

The one option you will reach for most is `show_indices = "cell"`, which
labels every cell with its `[row, column]` subscript. This is the
picture that makes `m[2, 4]` finally click.

``` r

paint_matrix(matrix(1:15, nrow = 3), show_indices = "cell")
```

![The same 3 by 5 grid of the numbers 1 through 15, now with each cell
labelled by its \[row, column\] subscript, from \[1, 1\] in the top-left
corner to \[3, 5\] in the
bottom-right.](paintr_files/figure-html/unnamed-chunk-9-1.png)

If the matrix has dimnames, they draw in the margins, and
`show_dimnames` chooses which by taking a character vector over
`"none"`, `"row"`, `"column"`, and `"all"`.

## Data frames

A data frame is the workhorse, and it gets the richest default picture,
with a header row of column names, the type of each column beneath it,
and a row-name gutter whenever the row names are meaningful.

``` r

paint_data_frame(head(mtcars))
```

![The first six rows of the mtcars data frame drawn as a table: a header
row of bare column names such as mpg, cyl, and disp, the column type
beneath each, and a row-name gutter down the left labelling each row as
an accessor, \["Mazda RX4", \] through \["Valiant", \]; numbers show
three significant figures with any trailing digits in
grey.](paintr_files/figure-html/unnamed-chunk-10-1.png)

Notice the numbers. paintr shows three significant figures by default,
and draws any trailing insignificant digits in **grey**, so a reader can
see the true value without being distracted by noise. `mtcars` has real
row names, so the gutter appears, while a fresh `iris` would not draw
one. Toggle the extra rows with `show_names`, `show_types`, and
`show_rownames`.
[`paint_df()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md)
is a shorter alias.

## Lists

A list is the flexible container, and its elements can have different
lengths and different types. paintr draws each element as a column.

``` r

paint_list(list(id = 1:3, tags = c("a", "b"), ok = c(TRUE, FALSE, NA)))
```

![A list of three elements drawn as three side-by-side columns headed
\$id, \$tags, and \$ok; id holds 1, 2, 3, tags holds a and b, and ok
holds TRUE, FALSE, NA. The tags column is one cell shorter, leaving a
ragged bottom edge because the elements do not share a
length.](paintr_files/figure-html/unnamed-chunk-11-1.png)

A data frame is just a list whose elements all share a length. Here is a
small data frame, and here it is again as a bare list after
[`as.list()`](https://rdrr.io/r/base/list.html) strips the class.

``` r

df <- data.frame(x = c(1.5, 2.5, 3.5), grp = c("a", "b", "c"))
paint_data_frame(df)
```

![A small data frame drawn as a table with two columns, x holding 1.5,
2.5, 3.5 and grp holding a, b, c, each headed by its bare column name
with the column type beneath; the elements share a length, so the table
is a clean rectangle.](paintr_files/figure-html/unnamed-chunk-12-1.png)

``` r

paint_list(as.list(df))
```

![The same data drawn as a bare list after as.list(): two columns headed
\$x and \$grp holding the same values 1.5, 2.5, 3.5 and a, b, c. Because
the two elements share a length, the list draws the same clean rectangle
a data frame does.](paintr_files/figure-html/unnamed-chunk-13-1.png)

The two pictures are identical, the same columns and the same clean
rectangle. The list draws that outline *only because* its elements share
a length, and that shared length is exactly what makes a list a data
frame. Shorten one element and the outline breaks with it, the ragged
edge marking an object that is no longer a data frame.

``` r

broken <- as.list(df)
broken$x <- broken$x[1:2]
paint_list(broken)
```

![The same list after shortening x to two values, 1.5 and 2.5, while grp
keeps three, a, b, c; the elements no longer line up, so the outline is
gone and the bottom edge is ragged, the picture of an object that is no
longer a data frame.](paintr_files/figure-html/unnamed-chunk-14-1.png)

The rectangle is a convenience, though, not the truth of the object. A
list is a bag of independent vectors that happen, here, to line up. Set
`gap` and the columns move apart to say so, dropping the outline that
marked them as a table.

``` r

paint_list(as.list(df), gap = 0.5)
```

![The same equal-length list drawn with gap = 0.5, which pushes the two
columns x and grp apart into separate boxed vectors with clear space
between them and no enclosing outline; the values are unchanged, but the
picture now reads as two independent columns rather than one
table.](paintr_files/figure-html/unnamed-chunk-15-1.png)

The label under each cell is an accessor you can type.
`show_indices = "cell"` writes the `[[j]][i]` subscript under every
value, which settles the single-versus-double-bracket question by
putting the working expression under the number it returns.

``` r

l <- list(a = c(10, 20, 30), b = c(1.5, 2.5))
paint_list(l, show_indices = "cell")
```

![A list of two elements headed \$a and \$b, holding 10, 20, 30 and 1.5,
2.5, drawn with each cell labelled by its list accessor, double brackets
to reach the element then a single bracket to pick the value, so
\[\[2\]\]\[2\] sits under 2.5 and \[\[1\]\]\[1\] under
10.](paintr_files/figure-html/unnamed-chunk-16-1.png)

Under the `2.5` it prints `l[[2]][2]`, double brackets to reach into the
list and a single bracket to pick the element.

paintr does not draw nesting, so a list inside a list collapses to a
grey `<list [2]>` token rather than recursing.

## Arrays

An array of three or more dimensions is a stack of matrices, and
[`paint_array()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-array.md)
lays that stack out as a row of blocks sharing one font size. (A 2-D
array is just a matrix, and draws like one.)

``` r

paint_array(array(1:24, dim = c(2, 3, 4)), show_indices = "cell")
```

![The numbers 1 through 24 drawn as a 2 by 3 by 4 array: four
side-by-side blocks, each a 2 by 3 matrix slice sharing one font size
and titled , , 1 through , , 4. Every cell is labelled with its full
three-number subscript, from \[1, 1, 1\] to \[2, 3,
4\].](paintr_files/figure-html/unnamed-chunk-17-1.png)

Each block is one slice, and the label under a cell is its full
subscript, `[2, 3, 4]`, a genuine 3-D index into the array.
`show_dimnames` works as it does for a matrix, with an added `"slice"`
lane. A genuinely huge array elides its middle, the same way the other
structures do once they pass their size caps.

When a stack has more slices than sit comfortably in one row,
`slices_per_row` wraps the blocks onto a grid, the same array laid out
to stay readable. Each block keeps its own slice title, so wrapping only
changes the reading order.

``` r

paint_array(array(1:24, dim = c(2, 3, 4)), slices_per_row = 2)
```

![The same 2 by 3 by 4 array, now with slices_per_row = 2, so the four
slice blocks are laid out in a two-by-two grid instead of a single row.
The blocks are titled , , 1 through , , 4 in left-to-right,
top-to-bottom reading order, and are larger and easier to read than in
one cramped row.](paintr_files/figure-html/unnamed-chunk-18-1.png)

## Picking a legible device size

Most defaults fit a normal plot window, but a large structure drawn in
full can shrink its text below the point where it is readable.
[`paint_size()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint_size.md)
tells you the device size that keeps everything legible, in a form you
can paste straight into a
[`png()`](https://rdrr.io/r/grDevices/png.html) call or a knitr chunk
header.

``` r

paint_size(head(mtcars))
#>  width height 
#>    4.4    1.8
```

The answer is in inches by default. Pass `units = "px"` for a
[`png()`](https://rdrr.io/r/grDevices/png.html) at a given `dpi`. When a
picture would be too small, paintr warns and quotes you these exact
numbers.

## Where next

That is the whole tour, five structures, two backends, one consistent
picture. From here:

- [`vignette("highlighting", package = "paintr")`](https://r-pkg.thecoatlessprofessor.com/paintr/articles/highlighting.md)
  shows how to spotlight rows, columns, and individual cells to draw the
  eye where a lesson needs it.
- [`vignette("base-vs-ggplot2", package = "paintr")`](https://r-pkg.thecoatlessprofessor.com/paintr/articles/base-vs-ggplot2.md)
  digs into the `paint_` and `gpaint_` split and when each backend pays
  off.
