# Highlight data

Build a logical mask that marks the cells to highlight in a subsequent
`paint_*()` / `gpaint_*()` call.

## Usage

``` r
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'numeric'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'integer'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'character'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'logical'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'complex'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'factor'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'Date'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'POSIXct'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'vector'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'matrix'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'array'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'table'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'list'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# S3 method for class 'data.frame'
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

# Default S3 method
highlight_data(x, rows = NULL, columns = NULL, locations = NULL, ...)

highlight_rows(x, rows = NULL)

highlight_columns(x, columns = NULL)

highlight_locations(x, locations = NULL)
```

## Arguments

- x:

  A vector, factor, matrix, or data frame.

- rows:

  A vector of valid row locations, given either as integer indices, as
  row names, or as a logical mask.

- columns:

  A vector of valid column locations, given either as integer indices,
  as column names, or as a logical mask.

- locations:

  An m by 2 matrix with points listed in row, column format for a 2D
  object or a vector of integer indices in a 1D format.

- ...:

  Additional values (not used)

## Value

A logical matrix or vector with the required rows and/or columns or
points set to `TRUE`. All other values are given as `FALSE`. The result
always has the same shape as `x`: a logical vector of `length(x)` for 1D
structures, and a logical matrix of `dim(x)` for matrices and data
frames.

## Supported structures

Methods exist for `numeric`, `integer`, `character`, `logical`,
`complex`, `factor`, `Date`, `POSIXct`, `matrix`, `array`, `table`,
`data.frame`, and a bare `list`. Anything else is an error.

A **list**'s mask is positions by elements, because that is how
[`paint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md)
draws it: `columns` select elements (by name, `names(x)`, as well as by
number) and `rows` select positions within them. It is as deep as the
deepest element. An **empty** list is refused, because
[`paint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md)
cannot draw one either.

A list carrying a class – `as.POSIXlt(Sys.time())`, an `lm`, a
[`t.test()`](https://rdrr.io/r/stats/t.test.html) result – is **not** a
list for these purposes and is refused, exactly as
[`paint_list()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-list.md)
refuses it. [`is.list()`](https://rdrr.io/r/base/list.html) is TRUE for
all of them, which is why the question is never asked that way; see the
note on `is_paint_list()`.

An **array** or a **table** of rank two or more is masked with its own
full shape: the mask of `Titanic` is a 4 x 2 x 2 x 2 logical array,
because that is what
[`paint_array()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-array.md)
draws. `rows` and `columns` select on the first two axes – the two a
*block* is made of – and mark that row (or column) of **every** slice;
`locations` takes a **full coordinate per point**, one column per
dimension, and reaches a single cell. Rank one is refused: nothing draws
it.

The governing invariant: **if a painter can draw a structure,
`highlight_data()` must be able to mask it** – and a *wrong* mask is
worse than an error, so a structure no painter accepts is refused
outright rather than reshaped into a mask nothing could consume. A 2D
`table` is drawn by
[`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md),
so it is masked here; `Titanic` is drawn by
[`paint_array()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-array.md),
so it is masked here. An n-D array used to *stop* for exactly the same
reason it is now *masked* – the invariant never changed, the set of
things a painter can draw did, and the two halves moved together.

There is deliberately no reliance on a `vector` method being
*dispatched*: `inherits(letters, "vector")` is `FALSE`, so
`highlight_data.vector()` is never selected by
[`UseMethod()`](https://rdrr.io/r/base/UseMethod.html) for an atomic
vector. The atomic methods are therefore fanned out explicitly, and each
one forwards to `highlight_data.vector()` by a direct call. `array` and
`table` are the same trap one type over: an atomic array dispatches on
`c("array", "integer", "numeric")`, so without an `array` method it
would land on `highlight_data.integer()` and be silently flattened, and
a `table` dispatches on `"table"` alone, so without a `table` method it
would land on `highlight_data.default()` and be refused despite being
paintable.

## See also

The painters that consume a mask, such as
[`paint_matrix()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-matrix.md)
and
[`paint_data_frame()`](https://r-pkg.thecoatlessprofessor.com/paintr/reference/paint-data-frame.md).

## Examples

``` r
## 2D Highlighting for Matrices ----
# Example data
x <- matrix(1:12, nrow = 4)

# Highlight points using a row, column pairing
locations <- rbind(
  c(1, 3),
  c(2, 2),
  c(4, 1)
)
highlight_locations(x, locations)
#>       [,1]  [,2]  [,3]
#> [1,] FALSE FALSE  TRUE
#> [2,] FALSE  TRUE FALSE
#> [3,] FALSE FALSE FALSE
#> [4,]  TRUE FALSE FALSE

# Highlight entries only in the 1st and 3rd rows.
highlight_rows(x, rows = c(1, 3))
#>       [,1]  [,2]  [,3]
#> [1,]  TRUE  TRUE  TRUE
#> [2,] FALSE FALSE FALSE
#> [3,]  TRUE  TRUE  TRUE
#> [4,] FALSE FALSE FALSE

# Highlight entries only in the first two rows:
highlight_rows(x, rows = 1:2)
#>       [,1]  [,2]  [,3]
#> [1,]  TRUE  TRUE  TRUE
#> [2,]  TRUE  TRUE  TRUE
#> [3,] FALSE FALSE FALSE
#> [4,] FALSE FALSE FALSE

# Highlight entries in the last column
highlight_columns(x, columns = ncol(x))
#>       [,1]  [,2] [,3]
#> [1,] FALSE FALSE TRUE
#> [2,] FALSE FALSE TRUE
#> [3,] FALSE FALSE TRUE
#> [4,] FALSE FALSE TRUE

# Highlight entries in the first column
highlight_columns(x, columns = 1)
#>      [,1]  [,2]  [,3]
#> [1,] TRUE FALSE FALSE
#> [2,] TRUE FALSE FALSE
#> [3,] TRUE FALSE FALSE
#> [4,] TRUE FALSE FALSE

# Highlight entries in the first column or first row.
highlight_data(x, rows = 1, columns = 1)
#>      [,1]  [,2]  [,3]
#> [1,] TRUE  TRUE  TRUE
#> [2,] TRUE FALSE FALSE
#> [3,] TRUE FALSE FALSE
#> [4,] TRUE FALSE FALSE

## 1D Highlighting for Vectors ----
vec <- c(3, NA, -1, 2, NaN, Inf, 42)
highlight_data(vec, locations = c(2, 4, 6))
#> [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE

# Character and logical vectors work the same way.
highlight_locations(letters[1:5], c(2, 4))
#> [1] FALSE  TRUE FALSE  TRUE FALSE
highlight_data(c(TRUE, FALSE, TRUE))
#> [1] FALSE FALSE FALSE

## 2D Highlighting for Data Frames ----
# Columns may be named instead of numbered.
highlight_columns(iris[1:5, ], "Sepal.Width")
#>       [,1] [,2]  [,3]  [,4]  [,5]
#> [1,] FALSE TRUE FALSE FALSE FALSE
#> [2,] FALSE TRUE FALSE FALSE FALSE
#> [3,] FALSE TRUE FALSE FALSE FALSE
#> [4,] FALSE TRUE FALSE FALSE FALSE
#> [5,] FALSE TRUE FALSE FALSE FALSE
highlight_rows(iris[1:5, ], rows = c(1, 3))
#>       [,1]  [,2]  [,3]  [,4]  [,5]
#> [1,]  TRUE  TRUE  TRUE  TRUE  TRUE
#> [2,] FALSE FALSE FALSE FALSE FALSE
#> [3,]  TRUE  TRUE  TRUE  TRUE  TRUE
#> [4,] FALSE FALSE FALSE FALSE FALSE
#> [5,] FALSE FALSE FALSE FALSE FALSE
highlight_locations(iris[1:5, ], rbind(c(1, 1), c(2, 5)))
#>       [,1]  [,2]  [,3]  [,4]  [,5]
#> [1,]  TRUE FALSE FALSE FALSE FALSE
#> [2,] FALSE FALSE FALSE FALSE  TRUE
#> [3,] FALSE FALSE FALSE FALSE FALSE
#> [4,] FALSE FALSE FALSE FALSE FALSE
#> [5,] FALSE FALSE FALSE FALSE FALSE
```
