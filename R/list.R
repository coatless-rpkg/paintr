# The list painters.
#
# There is almost nothing here, and that is the whole claim: a data frame IS a list
# whose elements happen to share a length, so a list is not a new picture -- it is
# the SAME picture with the shared-length constraint removed.
#
# The trick that buys it is one reuse. An element's NAME goes in the `header` lane
# that `R/cells.R` already builds for a data frame -- the lane that exists because a
# data frame's columns are named variables, which a list's elements are too -- and
# the elements are laid out as COLUMNS. Then NO RECTANGLE EVER SPANS MORE THAN ONE
# CELL: there is no rowspan, no new tier, no new `kind`, and `R/layout.R`,
# `R/render-base.R` and `R/render-grid.R` are untouched, to the line.
#
# What is genuinely new is `col_len` -- the drawn length of each column -- and it
# lives entirely inside `paint_cells()`. `col_len` is to RAGGEDNESS what `fmt_group`
# is to FORMATTING: every rectangular structure is the degenerate case where every
# entry is equal, so raggedness is a VALUE and not a code path.
#
# `max_rows = 10` / `max_cols = 8`, not the matrix's 20/15: a matrix's cells are
# square, and a list's are as wide as a NAME. The pair is calibrated so that a
# default call clears the legibility floor even when every element is named at the
# full 12 characters (8 x 10 at 12 chars fits at 7.48pt on a 7x5in device, against a
# floor of 5).

## Base list ----

#' Visualize Data Inside of a List
#'
#' Generate a graph showing the contents of a list.
#'
#' **This picture does not draw NESTING.** A sublist is drawn as a single grey token
#' saying what it is -- `<list [2]>` -- and its contents are not drawn at all.
#' Nesting is the hard part of lists (`l$a$b`, "why is my result a list of lists"),
#' and this painter refuses to teach it rather than teach it badly: a geometric
#' sub-grid inside a cell would not extend the layout engine, it would delete it
#' (every column has ONE width, indexed by integer column, and a fractional row is
#' silently truncated). The refusal is deliberate and it is stated here rather than
#' discovered in a picture. The same is true of any element that is not a plain
#' one-dimensional vector: a matrix element draws as `<int [2 x 2]>`, a data frame
#' element as `<df [5 x 3]>`.
#'
#' `paint_list()` draws on the current base graphics device. `gpaint_list()` returns
#' a `ggplot` object.
#'
#' **A data frame IS a list whose elements happen to share a length.** That is what
#' this picture is for. Draw `paint_data_frame(data.frame(a = 1:3, b = 4:6))` and
#' `paint_list(list(a = 1:3, b = 4:6))` side by side and they are the same picture;
#' then draw `paint_list(list(a = 1:3, b = 4))` and watch the rectangle break. The
#' shared length is the only thing a data frame adds, and the ragged block is the
#' proof.
#'
#' Each element is its own formatting unit, exactly as a data frame's column is: a
#' `1e15` in one element will not flip another into scientific notation.
#'
#' @param data            A bare `list`. A list carrying a class --
#'                        `as.POSIXlt(Sys.time())`, an `lm`, a `t.test()` result --
#'                        is refused: `is.list()` is TRUE for all of them, but a
#'                        datetime drawn as eleven ragged columns of `sec`, `min`,
#'                        `hour`, ... is a wrong picture, not a picture of a list.
#' @param summarise       Draw every element as ONE cell saying what it is
#'                        (`<int [3]>`, `<chr [1]>`) instead of one cell per value.
#'                        Default: `FALSE`. It is the structure of the list at a
#'                        glance, and it is what a 40-element list wants.
#' @param show_indices    Draw `[[j]][i]` under each value (`"cell"` or `"all"`), or
#'                        nothing (`"none"`, the default).
#'
#'   `l[[2]][3]` is the expression students get wrong most often -- `l[2]` is a
#'   list, `l[[2]]` is the vector, `l[[2]][3]` is the value -- and this lane prints
#'   it under the number it returns. A summarised element gets `[[j]]`, because that
#'   is the accessor that returns the element itself.
#'
#'   There is deliberately **no** `[i]` gutter down the left. In a data frame,
#'   reading across a row is the whole point: `df[[1]][2]` and `df[[2]][2]` are one
#'   record. **In a list that is false**, and a lane that teaches it would be a lane
#'   that teaches a falsehood.
#' @param show_names      Draw the element-name row. Default: `TRUE`. A named
#'                        element is labelled `$a`; an unnamed one is labelled
#'                        `[[2]]`, exactly as `print()` does it. (This per-element
#'                        fallback is the one place in the package where a label lane
#'                        holds two kinds of thing at once -- a list is the only
#'                        structure whose parts are named one at a time.)
#' @param show_types      Draw the type-tag row (`<dbl>`, `<chr>`, ...) under the
#'                        element names. Default: `TRUE`.
#' @param name_align      How the element names sit over their column: `"center"`
#'                        (the default), `"left"` or `"right"`.
#' @param type_align      How the type tags sit over their column. Independent of
#'                        `name_align`. Neither moves the values, which keep their
#'                        own alignment.
#' @param highlight_area  Logical matrix marking the cells to fill: POSITIONS by
#'                        ELEMENTS, as deep as the deepest element. Build it with
#'                        [highlight_columns()] (which selects elements, by name as
#'                        well as by number) and [highlight_rows()] (which selects
#'                        positions within them). A length-one logical is recycled.
#'                        Default: `NULL`, which highlights nothing.
#' @param max_rows        Elide the middle of an element longer than this. Default:
#'                        `10`. It is asked of each element SEPARATELY, so a length-2
#'                        element beside a length-40 one draws no `"..."` -- it is
#'                        hiding nothing.
#' @param max_cols        Elide the middle of the list when it has more elements than
#'                        this. Default: `8`, because a list's columns are as wide as
#'                        a name.
#' @inheritParams paint_matrix
#' @param graph_subtitle  Subtitle to appear immediately under the graph title.
#'                        `NULL` (the default) describes the data: how many elements,
#'                        the range of their lengths, and the class. `NA` or `""`
#'                        draws no subtitle.
#'
#' @return
#' `paint_list()` invisibly returns the resolved cell table. See [paint_matrix()]
#' for its components.
#' `gpaint_list()` returns a `ggplot` object.
#'
#' @section The ragged block has no outline:
#' Every other painter draws a heavy border around the block of values. A list does
#' not, and the reason is that the block is not a rectangle. The heavy border would
#' be the BOUNDING BOX of the drawn cells, so on a 4/1/3 list it would run down to
#' the bottom of the deepest element and the length-1 element would sit at the top of
#' a tall, empty, heavily-boxed column -- a box drawn around cells that do not exist.
#' The cells keep their own borders, so the block still reads as a block; it just
#' reads as the ragged block it actually is.
#'
#' @section The ggplot object is a shell:
#' `gpaint_list()` returns a real `ggplot` object -- `+ theme()`, `ggsave()`,
#' `print()` and knitr chunks all work -- but its panel is drawn *entirely* by a
#' custom grid grob, held in a single `annotation_custom()` over a meaningless
#' `0..1` coordinate system. There is no `aes()`, no geom and no scale carrying any
#' meaning, so `ggplot_build()` sees an empty layer and `+ scale_fill_*()` has no
#' effect on the drawing. Use `highlight_area` and `highlight_color` to fill cells.
#'
#' @rdname paint-list
#' @export
#' @examples
#' # Base graphics
#'
#' # A list is a bag of vectors, and they need not be the same length.
#' paint_list(list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA)))
#'
#' # A data frame IS a list whose elements happen to share a length. These two
#' # pictures are the same picture.
#' paint_list(list(a = 1:3, b = 4:6))
#' paint_data_frame(data.frame(a = 1:3, b = 4:6))
#'
#' # An unnamed element is labelled the way print() labels it.
#' paint_list(list(1:3, b = letters[1:2]))
#'
#' # The accessor students get wrong, printed under the value it returns.
#' paint_list(list(a = 1:4, b = c(2.5, 3.5)), show_indices = "cell")
#'
#' # A sublist is NOT drawn. It says what it is and stops there.
#' paint_list(list(a = 1:3, b = list(1, 2)))
#'
#' # The structure of a long list, at a glance.
#' paint_list(list(a = 1:40, b = letters, c = matrix(1:4, 2)), summarise = TRUE)
#'
#' # Highlight an element by name.
#' l <- list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA))
#' paint_list(l, highlight_area = highlight_columns(l, "c"))
paint_list <- function(
    data,
    summarise = FALSE,
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
    show_all = FALSE,
    fontsize = NULL,
    family = "mono",
    show_types = TRUE,
    show_names = TRUE,
    name_align = c("center", "left", "right"),
    type_align = c("center", "left", "right")) {
  force(graph_title)

  if (!is_paint_list(data)) {
    stop("Please double-check the data supplied is of a `list` type.")
  }
  show_indices <- check_lanes(
    show_indices, c("none", "cell", "all"), "show_indices",
    example = c("none", "cell")
  )
  subtle_digits <- match.arg(subtle_digits)
  name_align <- match.arg(name_align)
  type_align <- match.arg(type_align)
  graph_subtitle <- resolve_subtitle(graph_subtitle, list_subtitle(data))

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
    show_all = show_all,
    fontsize = fontsize,
    family = family,
    summarise = summarise,
    show_names = show_names,
    show_types = show_types,
    name_align = name_align,
    type_align = type_align
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

## ggplot2 list ----

#' @rdname paint-list
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' # ggplot2 graphics ----
#'
#' gpaint_list(list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA)))
#'
#' gpaint_list(list(a = 1:40, b = letters), summarise = TRUE)
gpaint_list <- function(
    data,
    summarise = FALSE,
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
    show_all = FALSE,
    fontsize = NULL,
    family = "mono",
    show_types = TRUE,
    show_names = TRUE,
    name_align = c("center", "left", "right"),
    type_align = c("center", "left", "right")) {
  force(graph_title)
  require_ggplot2()

  if (!is_paint_list(data)) {
    stop("Please double-check the data supplied is of a `list` type.")
  }
  show_indices <- check_lanes(
    show_indices, c("none", "cell", "all"), "show_indices",
    example = c("none", "cell")
  )
  subtle_digits <- match.arg(subtle_digits)
  name_align <- match.arg(name_align)
  type_align <- match.arg(type_align)
  graph_subtitle <- resolve_subtitle(graph_subtitle, list_subtitle(data))

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
    show_all = show_all,
    fontsize = fontsize,
    family = family,
    summarise = summarise,
    show_names = show_names,
    show_types = show_types,
    name_align = name_align,
    type_align = type_align
  )

  gpaint_skin(prep, graph_title, graph_subtitle)
}
