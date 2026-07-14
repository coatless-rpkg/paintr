# The array painters.
#
# There is very little here, and what there is is a type check and a subtitle. The
# work is in `array_cells()` (R/cells.R), and the reason it is there rather than
# here is the whole architectural claim of this file: AN ARRAY IS NOT DRAWN IN
# PANELS. It is drawn as ONE cell table, with the slices faceted into it, because
# `paint_resolve()` fits a font to one table and k panels would fit k fonts. See the
# note above `array_cells()`.
#
# The consequence for this file is that it is the same file as `matrix.R`. It builds
# a cell table, it hands it to a renderer, and it never once mentions a slice.

## Base array ----

#' Visualize Data Inside of an Array
#'
#' Generate a graph showing the contents of an array of any rank, laid out the way
#' `print()` lays one out: a block per slice, each under the `, , Male, Child` title
#' that names it.
#'
#' `paint_array()` draws on the current base graphics device. `gpaint_array()`
#' returns a `ggplot` object.
#'
#' **A matrix is an array, and this draws it.** `is.array(matrix(1:4, 2))` is TRUE,
#' and a rank-two array is simply the case of this picture with one block and no
#' title to put over it -- so `paint_array(m)` and [paint_matrix()] draw the same
#' picture of the same matrix, down to the cell, because they run the same code.
#'
#' @section The label under the cell is the expression you type:
#' This is the promise the package is built on, and an array is where it is easiest
#' to break. On a 3-D array, `a[1, ]` and `a[2, 3]` are not "shorthand" -- they are
#' **errors**:
#'
#' ```
#' a <- array(1:24, c(2, 3, 4))
#' a[1, ]      # Error in a[1, ] : incorrect number of dimensions
#' a[2, 3]     # Error in a[2, 3] : incorrect number of dimensions
#' a[2, 3, 4]  # 24
#' ```
#'
#' So every index this picture draws carries the array's full subscript arity, with
#' the slice filled in from the block the label sits in: the row gutter reads
#' `[1, , 3]`, the column lane `[, 2, 3]`, and the in-cell index `[1, 2, 3]`. Copy
#' any label off the picture, type it, and it returns the thing it was drawn beside.
#' A 4-D array reads `[1, 2, 3, 2]`. A matrix reads `[1, 2]`, exactly as
#' [paint_matrix()] does.
#'
#' `show_indices = "all"` is the call this section exists for.
#'
#' @section The whole array is one formatting unit:
#' A `1e15` in the third slice flips the *first* slice into scientific notation, and
#' the same number is drawn the same way in every block. An array is one homogeneous
#' object -- `a[1, 1, 1]` and `a[2, 3, 4]` measure the same thing in the same units
#' -- and R's own `print()` formats it to one common width across every slice. This
#' is the opposite of a data frame, whose columns are separate *variables* and are
#' therefore formatted separately.
#'
#' @param data            An `array`. A `matrix` and a `table` are arrays and are
#'                        drawn as such. Rank 1 is not drawn.
#' @param show_indices    Display indices based on location. A character vector, so
#'                        several kinds of index can be asked for at once:
#'                        `"none"` (the default), `"cell"` (`[1, 2, 3]`, inside the
#'                        cell), `"row"` (`[1, , 3]`, to the left of each block),
#'                        `"column"` (`[, 2, 3]`, above each block), or `"all"`.
#'
#'   Every one of them is a subscript expression that RUNS -- see the section above.
#' @param show_dimnames   Which of the array's `dimnames()` to draw. A character
#'                        vector, because `dimnames()` is a *list* with one slot per
#'                        axis: `"none"`, `"row"`, `"column"`, `"slice"`, or `"all"`
#'                        (the default).
#'
#'   `"slice"` is what puts the names in the block titles: with it, a block of
#'   `Titanic` is titled `, , Child, No`; without it, `, , 1, 1`. An index lane the
#'   caller asks for **wins** the axis it names, exactly as it does for a matrix.
#' @param max_slices      Elide the slice axes when the array has more slices than
#'                        this along either of them, drawing a `"..."` block in
#'                        place of the hidden ones. Default: `4`.
#'
#'   The picture is `max_slices` matrices wide, so this is the knob that costs the
#'   most: at `4`, `Titanic` and `HairEyeColor` draw whole and `UCBAdmissions` draws
#'   three of its six departments and says `# 3 more slices`. Raise it (or set
#'   `show_all`) to see them all, at a smaller size.
#' @param max_rows,max_cols Elide the middle of each *block* when the array has more
#'                        rows or columns than this. Default: `10` by `8` -- tighter
#'                        than [paint_matrix()]'s `20` by `15`, because the picture
#'                        is several blocks wide and they share the device between
#'                        them. As always, the decision is made on the dimensions
#'                        alone, with no device consulted.
#' @param highlight_area  Logical array the same shape as `data`, marking the cells
#'                        to fill. Build it with [highlight_data()], which masks an
#'                        array of any rank. A length-one logical is recycled.
#'                        Default: `NULL`, which highlights nothing.
#' @param graph_subtitle  Subtitle to appear immediately under the graph title.
#'                        `NULL` (the default) describes the data: its full shape
#'                        (`4 x 2 x 2 x 2`) and its class. `NA` or `""` draws no
#'                        subtitle.
#' @inheritParams paint_matrix
#'
#' @return
#' `paint_array()` invisibly returns the resolved cell table. See [paint_matrix()]
#' for its components.
#' `gpaint_array()` returns a `ggplot` object.
#'
#' @section The slices are not panels:
#' They are blocks in a single cell table, and that is not an implementation detail.
#' The font size is fitted to one table, so every block in the picture is drawn at
#' the same size. Laying the slices out as real graphics panels -- `par(mfrow =)`,
#' `layout()`, or one grid viewport each -- fits a *separate* font to each one, and
#' three slices of one array come out at 24, 12.4 and 23.1 points: the same number,
#' drawn at half the size, two inches to the left. Equal panels, unequal fonts.
#'
#' @rdname paint-array
#' @export
#' @examples
#' # Base graphics
#'
#' # A 3-D array lays its slabs out in one line.
#' paint_array(array(1:24, c(2, 3, 4)))
#'
#' # A 4-D contingency table is a real grid of blocks: across is the third
#' # dimension, down is the fourth.
#' paint_array(Titanic)
#'
#' # The accessor lesson. Every label is an expression that runs: the value under
#' # `[2, 3, 2]` is exactly what `a[2, 3, 2]` returns.
#' paint_array(array(1:12, c(2, 3, 2)), show_indices = "all")
#'
#' # A matrix IS an array, and this draws it -- the same picture paint_matrix() does.
#' paint_array(matrix(1:6, nrow = 2))
#'
#' \donttest{
#' # Highlight a row of every slice.
#' paint_array(Titanic, highlight_area = highlight_data(Titanic, rows = 1))
#'
#' # Six departments, three drawn: the gap is always drawn, on the slice axis too.
#' paint_array(UCBAdmissions)
#' }
paint_array <- function(
    data,
    show_indices = "none",
    highlight_area = NULL,
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = NULL,
    sigfig = 3L,
    subtle_digits = c("insignificant", "rounded", "none"),
    max_chars = 12L,
    max_rows = 10L,
    max_cols = 8L,
    max_slices = 4L,
    show_all = FALSE,
    fontsize = NULL,
    family = "mono",
    show_dimnames = "all",
    max_name_chars = 8L) {
  # `graph_title` is a promise over `substitute(data)`, so it has to be forced here,
  # in the frame whose `data` the user actually named.
  force(graph_title)

  if (!is_paint_array(data)) {
    stop("Please double-check the data supplied is of an `array` type.")
  }
  show_indices <- check_show_indices(show_indices)
  show_dimnames <- check_lanes(
    show_dimnames, c("none", "row", "column", "slice", "all"), "show_dimnames"
  )
  subtle_digits <- match.arg(subtle_digits)
  graph_subtitle <- resolve_subtitle(graph_subtitle, array_subtitle(data))

  prep <- painter_prep(
    data = data,
    show_indices = show_indices,
    highlight_area = highlight_area,
    highlight_color = highlight_color,
    sigfig = sigfig,
    subtle_digits = subtle_digits,
    max_chars = max_chars,
    max_rows = max_rows,
    max_cols = max_cols,
    max_slices = max_slices,
    show_all = show_all,
    fontsize = fontsize,
    family = family,
    show_dimnames = show_dimnames,
    max_name_chars = max_name_chars
  )

  invisible(render_base(
    prep$cells, prep$col_w, prep$n_row,
    opts = prep$opts,
    graph_title = graph_title,
    graph_subtitle = graph_subtitle,
    note = prep$note,
    warn_floor = prep$warn_floor
  ))
}

## ggplot2 array ----

#' @rdname paint-array
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' # ggplot2 graphics ----
#'
#' gpaint_array(array(1:24, c(2, 3, 4)))
#'
#' gpaint_array(HairEyeColor)
gpaint_array <- function(
    data,
    show_indices = "none",
    highlight_area = NULL,
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = NULL,
    sigfig = 3L,
    subtle_digits = c("insignificant", "rounded", "none"),
    max_chars = 12L,
    max_rows = 10L,
    max_cols = 8L,
    max_slices = 4L,
    show_all = FALSE,
    fontsize = NULL,
    family = "mono",
    show_dimnames = "all",
    max_name_chars = 8L) {
  force(graph_title)
  require_ggplot2()

  if (!is_paint_array(data)) {
    stop("Please double-check the data supplied is of an `array` type.")
  }
  show_indices <- check_show_indices(show_indices)
  show_dimnames <- check_lanes(
    show_dimnames, c("none", "row", "column", "slice", "all"), "show_dimnames"
  )
  subtle_digits <- match.arg(subtle_digits)
  graph_subtitle <- resolve_subtitle(graph_subtitle, array_subtitle(data))

  prep <- painter_prep(
    data = data,
    show_indices = show_indices,
    highlight_area = highlight_area,
    highlight_color = highlight_color,
    sigfig = sigfig,
    subtle_digits = subtle_digits,
    max_chars = max_chars,
    max_rows = max_rows,
    max_cols = max_cols,
    max_slices = max_slices,
    show_all = show_all,
    fontsize = fontsize,
    family = family,
    show_dimnames = show_dimnames,
    max_name_chars = max_name_chars
  )

  gpaint_skin(prep, graph_title, graph_subtitle)
}
