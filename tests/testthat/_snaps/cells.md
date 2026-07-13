# the cell table snapshots: a numeric matrix with every index lane

    Code
      paint_cells(m, show_indices = "all")
    Output
          i  j row col fmt_group    sig insig   head tail    ink  fill border   align
      1  NA NA   2   2        NA                           black  <NA>  black  center
      2  NA  1   1   2        NA  [, 1]        [, 1]      grey40  <NA>   <NA>  center
      3  NA  2   1   3        NA  [, 2]        [, 2]      grey40  <NA>   <NA>  center
      4   1 NA   2   1        NA  [1, ]        [1, ]      grey40  <NA>   <NA>   right
      5   2 NA   3   1        NA  [2, ]        [2, ]      grey40  <NA>   <NA>   right
      6   3 NA   4   1        NA  [3, ]        [3, ]      grey40  <NA>   <NA>   right
      7   1  1   2   2         1      1            1       black white  black decimal
      8   2  1   3   2         1  0.333            0 .333  black white  black decimal
      9   3  1   4   2         1    123  457. 123457    .  black white  black decimal
      10  1  2   2   3         1     20           20       black white  black decimal
      11  2  2   3   3         1     -1           -1       black white  black decimal
      12  3  2   4   3         1     NA           NA         red white  black decimal
      13  1  1   2   2        NA [1, 1]       [1, 1]      grey50  <NA>   <NA>  center
      14  2  1   3   2        NA [2, 1]       [2, 1]      grey50  <NA>   <NA>  center
      15  3  1   4   2        NA [3, 1]       [3, 1]      grey50  <NA>   <NA>  center
      16  1  2   2   3        NA [1, 2]       [1, 2]      grey50  <NA>   <NA>  center
      17  2  2   3   3        NA [2, 2]       [2, 2]      grey50  <NA>   <NA>  center
      18  3  2   4   3        NA [3, 2]       [3, 2]      grey50  <NA>   <NA>  center
         size_rel dy_rel   fit      kind
      1       1.0    0.0 FALSE   outline
      2       0.8    0.0  TRUE  collabel
      3       0.8    0.0  TRUE  collabel
      4       0.8    0.0  TRUE  rowlabel
      5       0.8    0.0  TRUE  rowlabel
      6       0.8    0.0  TRUE  rowlabel
      7       1.0    0.0  TRUE     value
      8       1.0    0.0  TRUE     value
      9       1.0    0.0  TRUE     value
      10      1.0    0.0  TRUE     value
      11      1.0    0.0  TRUE     value
      12      1.0    0.0  TRUE     value
      13      0.7   -0.3  TRUE cellindex
      14      0.7   -0.3  TRUE cellindex
      15      0.7   -0.3  TRUE cellindex
      16      0.7   -0.3  TRUE cellindex
      17      0.7   -0.3  TRUE cellindex
      18      0.7   -0.3  TRUE cellindex

# the cell table snapshots: a data frame with names and types

    Code
      paint_cells(df)
    Output
          i  j row col fmt_group   sig insig  head tail    ink  fill border   align
      1  NA NA   3   1        NA                         black  <NA>  black  center
      2  NA  1   1   1        NA     n           n       black  <NA>   <NA>   right
      3  NA  2   1   2        NA     s           s       black  <NA>   <NA>    left
      4  NA  3   1   3        NA     l           l       black  <NA>   <NA>   right
      5  NA  1   2   1        NA <dbl>       <dbl>      grey50  <NA>   <NA>   right
      6  NA  2   2   2        NA <chr>       <chr>      grey50  <NA>   <NA>    left
      7  NA  3   2   3        NA <lgl>       <lgl>      grey50  <NA>   <NA>   right
      8   1  1   3   1         1   1.5           1   .5  black white  black decimal
      9   2  1   4   1         1  22.2          22   .2  black white  black decimal
      10  1  2   3   2         2     a           a       black white  black    left
      11  2  2   4   2         2    bb          bb       black white  black    left
      12  1  3   3   3         3  TRUE        TRUE       black white  black   right
      13  2  3   4   3         3    NA          NA         red white  black   right
         size_rel dy_rel   fit    kind
      1       1.0      0 FALSE outline
      2       0.9      0  TRUE  header
      3       0.9      0  TRUE  header
      4       0.9      0  TRUE  header
      5       0.8      0  TRUE    type
      6       0.8      0  TRUE    type
      7       0.8      0  TRUE    type
      8       1.0      0  TRUE   value
      9       1.0      0  TRUE   value
      10      1.0      0  TRUE   value
      11      1.0      0  TRUE   value
      12      1.0      0  TRUE   value
      13      1.0      0  TRUE   value

# the cell table snapshots: an elided matrix

    Code
      cells
    Output
          i  j row col fmt_group sig insig head tail    ink  fill border   align
      1  NA NA   1   1        NA                      black  <NA>  black  center
      2   1  1   1   1         1   1          1       black white  black decimal
      3   2  1   2   1         1   2          2       black white  black decimal
      4  30  1   4   1         1  30         30       black white  black decimal
      5   1  2   1   2         1  31         31       black white  black decimal
      6   2  2   2   2         1  32         32       black white  black decimal
      7  30  2   4   2         1  60         60       black white  black decimal
      8   1 30   1   4         1 871        871       black white  black decimal
      9   2 30   2   4         1 872        872       black white  black decimal
      10 30 30   4   4         1 900        900       black white  black decimal
      11 NA NA   3   1        NA ...        ...      grey50  <NA>   <NA>  center
      12 NA NA   3   2        NA ...        ...      grey50  <NA>   <NA>  center
      13 NA NA   1   3        NA ...        ...      grey50  <NA>   <NA>  center
      14 NA NA   2   3        NA ...        ...      grey50  <NA>   <NA>  center
      15 NA NA   3   3        NA ...        ...      grey50  <NA>   <NA>  center
      16 NA NA   4   3        NA ...        ...      grey50  <NA>   <NA>  center
      17 NA NA   3   4        NA ...        ...      grey50  <NA>   <NA>  center
         size_rel dy_rel   fit     kind
      1         1      0 FALSE  outline
      2         1      0  TRUE    value
      3         1      0  TRUE    value
      4         1      0  TRUE    value
      5         1      0  TRUE    value
      6         1      0  TRUE    value
      7         1      0  TRUE    value
      8         1      0  TRUE    value
      9         1      0  TRUE    value
      10        1      0  TRUE    value
      11        1      0 FALSE ellipsis
      12        1      0 FALSE ellipsis
      13        1      0 FALSE ellipsis
      14        1      0 FALSE ellipsis
      15        1      0 FALSE ellipsis
      16        1      0 FALSE ellipsis
      17        1      0 FALSE ellipsis

---

    Code
      column_widths(cells)
    Output
      [1] 1.35 1.35 1.35 1.35

# the cell table snapshots: a highlighted vector

    Code
      paint_cells(c(3, NA, -1, NaN, Inf), highlight_area = highlight_locations(c(3,
        NA, -1, NaN, Inf), c(2, 4)), show_indices = "outside")
    Output
          i  j row col fmt_group sig insig head tail    ink         fill border
      1  NA NA   1   2        NA                      black         <NA>  black
      2   1 NA   1   1        NA [1]        [1]      grey40         <NA>   <NA>
      3   2 NA   2   1        NA [2]        [2]      grey40         <NA>   <NA>
      4   3 NA   3   1        NA [3]        [3]      grey40         <NA>   <NA>
      5   4 NA   4   1        NA [4]        [4]      grey40         <NA>   <NA>
      6   5 NA   5   1        NA [5]        [5]      grey40         <NA>   <NA>
      7   1  1   1   2         1   3          3       black        white  black
      8   2  1   2   2         1  NA         NA         red lemonchiffon  black
      9   3  1   3   2         1  -1         -1       black        white  black
      10  4  1   4   2         1 NaN        NaN        blue lemonchiffon  black
      11  5  1   5   2         1 Inf        Inf        blue        white  black
           align size_rel dy_rel   fit     kind
      1   center      1.0      0 FALSE  outline
      2    right      0.8      0  TRUE rowlabel
      3    right      0.8      0  TRUE rowlabel
      4    right      0.8      0  TRUE rowlabel
      5    right      0.8      0  TRUE rowlabel
      6    right      0.8      0  TRUE rowlabel
      7  decimal      1.0      0  TRUE    value
      8  decimal      1.0      0  TRUE    value
      9  decimal      1.0      0  TRUE    value
      10 decimal      1.0      0  TRUE    value
      11 decimal      1.0      0  TRUE    value

