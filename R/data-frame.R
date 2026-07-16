# The data frame painters. BUG 4: `paint_matrix(df)` and `paint_vector(df)` both
# stopped, and there was no third painter.
#
# There is almost nothing here, and that is the point. A data frame is not a third
# code path -- it is the same cell builder with a different `fmt_group`:
# `seq_len(n_col)` instead of `rep(1L, n_col)`, so each column formats itself. The
# structural difference is expressed as DATA, not as a branch, which is why these
# painters cannot drift away from the matrix ones.
#
# The extra formals are the only genuinely new behaviour: a data frame gets a
# column-name row and a `<dbl>`/`<chr>` type row (`show_names`, `show_types`),
# and each of those two lanes is aligned on its own (`name_align`, `type_align`).
# A label names the whole column, so it is centred over it by default -- it does
# not inherit the values' alignment, which exists to anchor DIGITS.
#
# `max_rows`/`max_cols` default to 10, not 20: a data frame's columns are wide.

## Base data frame ----

# The shared body of both data frame painters. See `matrix_prep()` for the pattern.
# `graph_title` stays out, so its `substitute(data)` promise is forced only in the
# painter's own frame.
#
# A data frame takes all three shorthands: `highlight_columns` names its columns
# (by name as well as by number), `highlight_rows` its rows, `highlight_locations`
# its cells, each built by the SAME `highlight_data()` the mask builders call.
df_prep <- function(data,
                    show_indices,
                    highlight_area,
                    highlight_color,
                    graph_subtitle,
                    sigfig,
                    subtle_digits,
                    max_chars,
                    max_rows,
                    max_cols,
                    show_all,
                    fontsize,
                    family,
                    palette,
                    show_types,
                    show_names,
                    show_rownames,
                    name_align,
                    type_align,
                    highlight_rows = NULL,
                    highlight_columns = NULL,
                    highlight_locations = NULL) {
  if (!is.data.frame(data)) {
    stop("Please double-check the data supplied is of a `data.frame` type.")
  }
  show_indices <- check_show_indices(show_indices)
  subtle_digits <- match.arg(subtle_digits, c("insignificant", "rounded", "none"))
  # A lane has exactly one alignment, so `match.arg()` fits. It would NOT fit
  # `show_indices`, which is deliberately a vector.
  name_align <- match.arg(name_align, c("center", "left", "right"))
  type_align <- match.arg(type_align, c("center", "left", "right"))
  # A data frame is a grid, so it takes the grid subtitle: rows, columns, class.
  # Three painters, one contract -- a default line that two of them drew and the
  # third did not would be drift of exactly the kind the shared cell table exists
  # to prevent.
  graph_subtitle <- resolve_subtitle(graph_subtitle, dims_subtitle(data))
  highlight_area <- resolve_highlight_area(
    data, highlight_area,
    rows = highlight_rows, columns = highlight_columns, locations = highlight_locations
  )

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
    palette = palette,
    show_names = show_names,
    show_types = show_types,
    show_rownames = show_rownames,
    name_align = name_align,
    type_align = type_align
  )

  list(prep = prep, graph_subtitle = graph_subtitle)
}

#' Visualize Data Inside of a Data Frame
#'
#' Generate a graph showing the contents of a data frame.
#'
#' `paint_data_frame()` draws on the current base graphics device.
#' `gpaint_data_frame()` returns a `ggplot` object. `paint_df()` and `gpaint_df()`
#' are aliases.
#'
#' Each column is its own formatting unit, because a data frame's columns are
#' independent variables: a `1e15` in one column will not flip another column into
#' scientific notation. (A matrix is the opposite -- one unit for the whole thing,
#' so that the same value looks identical in every cell.)
#'
#' @param data            An object that has the class of `data.frame`.
#' @param show_types      Draw the type-tag row (`<dbl>`, `<chr>`, ...) under the
#'                        column names. Default: `TRUE`.
#' @param show_names      Draw the column-name row. Default: `TRUE`.
#' @param show_rownames   Draw the row names, in a gutter to the left of the frame.
#'                        `NULL` (the default) draws them when the frame has names
#'                        of its own: `mtcars` does ("Mazda RX4"), and `iris` does
#'                        not -- a gutter of `1, 2, 3` is the row's position, not
#'                        data about it. `TRUE` draws whatever `rownames()` returns;
#'                        `FALSE` draws nothing.
#'
#'   The gutter is what teaches the classic confusion: the car's name is **not** a
#'   column of `mtcars`, which is why `mtcars$name` is `NULL`. `show_indices = "row"`
#'   wins the same lane, and draws `[1, ]` instead.
#' @param name_align      How the column names sit over their column: `"center"`
#'                        (the default), `"left"` or `"right"`.
#' @param type_align      How the type tags sit over their column: `"center"`
#'                        (the default), `"left"` or `"right"`. Independent of
#'                        `name_align`.
#'
#'                        Both control the label lanes *only*. The values keep
#'                        their own alignment whatever the labels are told to do:
#'                        a numeric column stays anchored on its decimal point, a
#'                        character column stays left, a logical column stays
#'                        right.
#' @inheritParams paint_matrix
#' @param max_rows,max_cols Elide the middle of the data frame when it has more
#'                        rows or columns than this. Default: `10`, because a data
#'                        frame's columns are wide.
#'
#' @return
#' `paint_data_frame()` invisibly returns the resolved cell table. See
#' [paint_matrix()] for its components.
#' `gpaint_data_frame()` returns a `ggplot` object.
#'
#' @section The ggplot object is a shell:
#' `gpaint_data_frame()` returns a real `ggplot` object -- `+ theme()`, `ggsave()`,
#' `print()` and knitr chunks all work -- but its panel is drawn *entirely* by a
#' custom grid grob, held in a single `annotation_custom()` over a meaningless
#' `0..1` coordinate system. There is no `aes()`, no geom and no scale carrying
#' any meaning, so:
#'
#' * `ggplot_build()` sees an empty layer.
#' * `+ scale_fill_*()`, `+ scale_x_*()` and friends have no effect on the drawing.
#'   Use `highlight_area` and `highlight_color` to fill cells.
#' * `+ geom_point()` would draw onto the `0..1` coordinate system, not onto the
#'   cells.
#'
#' This is not a shortcut around ggplot2. The cell text is fitted to the device at
#' *draw* time, which no geom can do, because a layer is built long before the
#' device size is known; and every number is drawn as two spans in two colors,
#' which `geom_text()` cannot do at all. A custom grob is the only mechanism that
#' can do either.
#'
#' @family painters
#' @rdname paint-data-frame
#' @export
#' @examples
#' # Base graphics
#'
#' paint_data_frame(head(iris, 5))
#'
#' # The type row can be turned off.
#' paint_data_frame(head(mtcars, 4), show_types = FALSE)
#'
#' # The two label lanes align independently. The values do not move.
#' paint_data_frame(head(iris, 5), name_align = "left", type_align = "right")
#'
#' # A frame with row names of its own draws them in a gutter: the car's name is
#' # not a column of mtcars, which is why `mtcars$name` is NULL. iris has no such
#' # names, and draws no gutter -- a lane of 1, 2, 3 is a position, not data.
#' paint_data_frame(head(mtcars, 4))
#'
#' # Long frames elide their middle and say so.
#' paint_data_frame(iris)
#'
#' # Highlight a column by name.
#' paint_data_frame(
#'   head(iris, 5),
#'   highlight_area = highlight_columns(head(iris, 5), "Sepal.Width")
#' )
paint_data_frame <- function(
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
    max_cols = 10L,
    show_all = FALSE,
    fontsize = NULL,
    family = "mono",
    palette = NULL,
    show_types = TRUE,
    show_names = TRUE,
    show_rownames = NULL,
    name_align = c("center", "left", "right"),
    type_align = c("center", "left", "right"),
    highlight_rows = NULL,
    highlight_columns = NULL,
    highlight_locations = NULL) {
  force(graph_title)

  p <- df_prep(
    data = data,
    show_indices = show_indices,
    highlight_area = highlight_area,
    highlight_color = highlight_color,
    graph_subtitle = graph_subtitle,
    sigfig = sigfig,
    subtle_digits = subtle_digits,
    max_chars = max_chars,
    max_rows = max_rows,
    max_cols = max_cols,
    show_all = show_all,
    fontsize = fontsize,
    family = family,
    palette = palette,
    show_types = show_types,
    show_names = show_names,
    show_rownames = show_rownames,
    name_align = name_align,
    type_align = type_align,
    highlight_rows = highlight_rows,
    highlight_columns = highlight_columns,
    highlight_locations = highlight_locations
  )

  invisible(render_base(
    p$prep$cells, p$prep$col_w, p$prep$n_row,
    opts = p$prep$opts,
    graph_title = graph_title,
    graph_subtitle = p$graph_subtitle,
    note = p$prep$note,
    warn_floor = p$prep$warn_floor
  ))
}

## ggplot2 data frame ----

#' @rdname paint-data-frame
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' # ggplot2 graphics ----
#'
#' gpaint_data_frame(head(iris, 5))
#'
#' gpaint_df(head(mtcars, 4))
gpaint_data_frame <- function(
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
    max_cols = 10L,
    show_all = FALSE,
    fontsize = NULL,
    family = "mono",
    palette = NULL,
    show_types = TRUE,
    show_names = TRUE,
    show_rownames = NULL,
    name_align = c("center", "left", "right"),
    type_align = c("center", "left", "right"),
    highlight_rows = NULL,
    highlight_columns = NULL,
    highlight_locations = NULL) {
  force(graph_title)
  require_ggplot2()

  p <- df_prep(
    data = data,
    show_indices = show_indices,
    highlight_area = highlight_area,
    highlight_color = highlight_color,
    graph_subtitle = graph_subtitle,
    sigfig = sigfig,
    subtle_digits = subtle_digits,
    max_chars = max_chars,
    max_rows = max_rows,
    max_cols = max_cols,
    show_all = show_all,
    fontsize = fontsize,
    family = family,
    palette = palette,
    show_types = show_types,
    show_names = show_names,
    show_rownames = show_rownames,
    name_align = name_align,
    type_align = type_align,
    highlight_rows = highlight_rows,
    highlight_columns = highlight_columns,
    highlight_locations = highlight_locations
  )

  gpaint_skin(p$prep, graph_title, p$graph_subtitle)
}

## Aliases ----
#
# Plain assignment, not a wrapper. A wrapper would evaluate
# `deparse(substitute(data))` one frame down and every title would silently become
# "Data Object: data".

#' @rdname paint-data-frame
#' @export
paint_df <- paint_data_frame

#' @rdname paint-data-frame
#' @export
gpaint_df <- gpaint_data_frame
