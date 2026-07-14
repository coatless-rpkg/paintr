# the cell table snapshots: a numeric matrix with every index lane

    Code
      paint_cells(m, show_indices = "all")
    Output
          i  j row col row_end col_end fmt_group    sig insig   head tail    ink
      1  NA NA   2   2       4       3        NA                           black
      2  NA  1   1   2       1       2        NA  [, 1]        [, 1]      grey40
      3  NA  2   1   3       1       3        NA  [, 2]        [, 2]      grey40
      4   1 NA   2   1       2       1        NA  [1, ]        [1, ]      grey40
      5   2 NA   3   1       3       1        NA  [2, ]        [2, ]      grey40
      6   3 NA   4   1       4       1        NA  [3, ]        [3, ]      grey40
      7   1  1   2   2       2       2         1      1            1       black
      8   2  1   3   2       3       2         1  0.333            0 .333  black
      9   3  1   4   2       4       2         1    123  457. 123457    .  black
      10  1  2   2   3       2       3         1     20           20       black
      11  2  2   3   3       3       3         1     -1           -1       black
      12  3  2   4   3       4       3         1     NA           NA         red
      13  1  1   2   2       2       2        NA [1, 1]       [1, 1]      grey50
      14  2  1   3   2       3       2        NA [2, 1]       [2, 1]      grey50
      15  3  1   4   2       4       2        NA [3, 1]       [3, 1]      grey50
      16  1  2   2   3       2       3        NA [1, 2]       [1, 2]      grey50
      17  2  2   3   3       3       3        NA [2, 2]       [2, 2]      grey50
      18  3  2   4   3       4       3        NA [3, 2]       [3, 2]      grey50
          fill border lwd   align size_rel dy_rel   fit      kind
      1   <NA>  black   2  center      1.0    0.0 FALSE   outline
      2   <NA>   <NA>   1  center      0.8    0.0  TRUE  collabel
      3   <NA>   <NA>   1  center      0.8    0.0  TRUE  collabel
      4   <NA>   <NA>   1   right      0.8    0.0  TRUE  rowlabel
      5   <NA>   <NA>   1   right      0.8    0.0  TRUE  rowlabel
      6   <NA>   <NA>   1   right      0.8    0.0  TRUE  rowlabel
      7  white  black   1 decimal      1.0    0.0  TRUE     value
      8  white  black   1 decimal      1.0    0.0  TRUE     value
      9  white  black   1 decimal      1.0    0.0  TRUE     value
      10 white  black   1 decimal      1.0    0.0  TRUE     value
      11 white  black   1 decimal      1.0    0.0  TRUE     value
      12 white  black   1 decimal      1.0    0.0  TRUE     value
      13  <NA>   <NA>   1  center      0.7   -0.3  TRUE cellindex
      14  <NA>   <NA>   1  center      0.7   -0.3  TRUE cellindex
      15  <NA>   <NA>   1  center      0.7   -0.3  TRUE cellindex
      16  <NA>   <NA>   1  center      0.7   -0.3  TRUE cellindex
      17  <NA>   <NA>   1  center      0.7   -0.3  TRUE cellindex
      18  <NA>   <NA>   1  center      0.7   -0.3  TRUE cellindex

# the cell table snapshots: a data frame with names and types

    Code
      paint_cells(df)
    Output
          i  j row col row_end col_end fmt_group   sig insig  head tail    ink  fill
      1  NA NA   3   1       4       3        NA                         black  <NA>
      2  NA  1   1   1       1       1        NA     n           n       black  <NA>
      3  NA  2   1   2       1       2        NA     s           s       black  <NA>
      4  NA  3   1   3       1       3        NA     l           l       black  <NA>
      5  NA  1   2   1       2       1        NA <dbl>       <dbl>      grey50  <NA>
      6  NA  2   2   2       2       2        NA <chr>       <chr>      grey50  <NA>
      7  NA  3   2   3       2       3        NA <lgl>       <lgl>      grey50  <NA>
      8   1  1   3   1       3       1         1   1.5           1   .5  black white
      9   2  1   4   1       4       1         1  22.2          22   .2  black white
      10  1  2   3   2       3       2         2     a           a       black white
      11  2  2   4   2       4       2         2    bb          bb       black white
      12  1  3   3   3       3       3         3  TRUE        TRUE       black white
      13  2  3   4   3       4       3         3    NA          NA         red white
         border lwd   align size_rel dy_rel   fit    kind
      1   black   2  center      1.0      0 FALSE outline
      2    <NA>   1  center      0.9      0  TRUE  header
      3    <NA>   1  center      0.9      0  TRUE  header
      4    <NA>   1  center      0.9      0  TRUE  header
      5    <NA>   1  center      0.8      0  TRUE    type
      6    <NA>   1  center      0.8      0  TRUE    type
      7    <NA>   1  center      0.8      0  TRUE    type
      8   black   1 decimal      1.0      0  TRUE   value
      9   black   1 decimal      1.0      0  TRUE   value
      10  black   1    left      1.0      0  TRUE   value
      11  black   1    left      1.0      0  TRUE   value
      12  black   1   right      1.0      0  TRUE   value
      13  black   1   right      1.0      0  TRUE   value

# the cell table snapshots: an elided matrix

    Code
      cells
    Output
          i  j row col row_end col_end fmt_group sig insig head tail    ink  fill
      1  NA NA   1   1       4       4        NA                      black  <NA>
      2   1  1   1   1       1       1         1   1          1       black white
      3   2  1   2   1       2       1         1   2          2       black white
      4  30  1   4   1       4       1         1  30         30       black white
      5   1  2   1   2       1       2         1  31         31       black white
      6   2  2   2   2       2       2         1  32         32       black white
      7  30  2   4   2       4       2         1  60         60       black white
      8   1 30   1   4       1       4         1 871        871       black white
      9   2 30   2   4       2       4         1 872        872       black white
      10 30 30   4   4       4       4         1 900        900       black white
      11 NA NA   3   1       3       1        NA ...        ...      grey50  <NA>
      12 NA NA   3   2       3       2        NA ...        ...      grey50  <NA>
      13 NA NA   1   3       1       3        NA ...        ...      grey50  <NA>
      14 NA NA   2   3       2       3        NA ...        ...      grey50  <NA>
      15 NA NA   3   3       3       3        NA ...        ...      grey50  <NA>
      16 NA NA   4   3       4       3        NA ...        ...      grey50  <NA>
      17 NA NA   3   4       3       4        NA ...        ...      grey50  <NA>
         border lwd   align size_rel dy_rel   fit     kind
      1   black   2  center        1      0 FALSE  outline
      2   black   1 decimal        1      0  TRUE    value
      3   black   1 decimal        1      0  TRUE    value
      4   black   1 decimal        1      0  TRUE    value
      5   black   1 decimal        1      0  TRUE    value
      6   black   1 decimal        1      0  TRUE    value
      7   black   1 decimal        1      0  TRUE    value
      8   black   1 decimal        1      0  TRUE    value
      9   black   1 decimal        1      0  TRUE    value
      10  black   1 decimal        1      0  TRUE    value
      11   <NA>   1  center        1      0 FALSE ellipsis
      12   <NA>   1  center        1      0 FALSE ellipsis
      13   <NA>   1  center        1      0 FALSE ellipsis
      14   <NA>   1  center        1      0 FALSE ellipsis
      15   <NA>   1  center        1      0 FALSE ellipsis
      16   <NA>   1  center        1      0 FALSE ellipsis
      17   <NA>   1  center        1      0 FALSE ellipsis

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
          i  j row col row_end col_end fmt_group sig insig head tail    ink
      1  NA NA   1   2       5       2        NA                      black
      2   1 NA   1   1       1       1        NA [1]        [1]      grey40
      3   2 NA   2   1       2       1        NA [2]        [2]      grey40
      4   3 NA   3   1       3       1        NA [3]        [3]      grey40
      5   4 NA   4   1       4       1        NA [4]        [4]      grey40
      6   5 NA   5   1       5       1        NA [5]        [5]      grey40
      7   1  1   1   2       1       2         1   3          3       black
      8   2  1   2   2       2       2         1  NA         NA         red
      9   3  1   3   2       3       2         1  -1         -1       black
      10  4  1   4   2       4       2         1 NaN        NaN        blue
      11  5  1   5   2       5       2         1 Inf        Inf        blue
                 fill border lwd   align size_rel dy_rel   fit     kind
      1          <NA>  black   2  center      1.0      0 FALSE  outline
      2          <NA>   <NA>   1   right      0.8      0  TRUE rowlabel
      3          <NA>   <NA>   1   right      0.8      0  TRUE rowlabel
      4          <NA>   <NA>   1   right      0.8      0  TRUE rowlabel
      5          <NA>   <NA>   1   right      0.8      0  TRUE rowlabel
      6          <NA>   <NA>   1   right      0.8      0  TRUE rowlabel
      7         white  black   1 decimal      1.0      0  TRUE    value
      8  lemonchiffon  black   1 decimal      1.0      0  TRUE    value
      9         white  black   1 decimal      1.0      0  TRUE    value
      10 lemonchiffon  black   1 decimal      1.0      0  TRUE    value
      11        white  black   1 decimal      1.0      0  TRUE    value

