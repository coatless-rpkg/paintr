
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paintr

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/paintr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/paintr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `paintr` is to draw different *R* data structures on graphs.

> \[!NOTE\]
>
> A previous version of the package was called `drawr`; however, another
> package with the same name was published on CRAN. As a result, the
> package was renamed to `paintr`.

## Installation

You can install the development version of drawr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("coatless-rpkg/paintr")
```

## Design

The package is designed to take advantage of base R graphics alongside
`ggplot2`. We’re providing two different implementations for each system
under the naming scheme of:

- `paint_*()`: base R graphics
- `gpaint_*()`: `ggplot2`

## Example

Take for instance we have a matrix that looks like so:

``` r
mat_3x5 = matrix(
  c(
   1, NA,    3,   4,  NaN, 
  NA,  7,    8,  -9,  10, 
 -11, 12, -Inf, -14,  NA
 ),
 ncol = 5, byrow = TRUE)

mat_3x5
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1   NA    3    4  NaN
#> [2,]   NA    7    8   -9   10
#> [3,]  -11   12 -Inf  -14   NA
```

What if we wanted to see the contents laid out with their indices or
specific cells highlighted?

``` r
# Load the library
library(paintr)

# Graphic of matrix data structure using base R graphics
paint_matrix(mat_3x5)
```

<img src="man/figures/README-base-example-1.png" width="100%" />

``` r
# Show the cell indices
paint_matrix(mat_3x5, show_indices = "cell")
```

<img src="man/figures/README-base-example-2.png" width="100%" />

``` r
# Show all indices
paint_matrix(mat_3x5, show_indices = "all")
```

<img src="man/figures/README-base-example-3.png" width="100%" />

``` r
# Highlight cells over a specific value
paint_matrix(mat_3x5, highlight_area = mat_3x5 > 4)
```

<img src="man/figures/README-base-example-4.png" width="100%" />

We can achieve similar results with the `ggplot2` function.

``` r
# Graphic of matrix data structure using base R graphics
gpaint_matrix(mat_3x5)
```

<img src="man/figures/README-ggplot2-example-1.png" width="100%" />

``` r
# Highlight cells in specific columns
gpaint_matrix(mat_3x5, 
             show_indices = c("row", "column"),
             highlight_area = highlight_columns(mat_3x5, columns = 2:4))
```

<img src="man/figures/README-ggplot2-example-2.png" width="100%" />
