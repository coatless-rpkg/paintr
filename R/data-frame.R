# The data frame painters. BUG 4: `paint_matrix(df)` and `paint_vector(df)` both
# stopped, and there was no third painter.
#
# There is almost nothing here, and that is the point. A data frame is not a third
# code path -- it is the same cell builder with a different `fmt_group`:
# `seq_len(n_col)` instead of `rep(1L, n_col)`, so each column formats itself. The
# structural difference is expressed as DATA, not as a branch, which is why these
# painters cannot drift away from the matrix ones.
#
# The two extra formals, `show_names` and `show_types`, are the only genuinely new
# behaviour: a data frame gets a column-name row and a `<dbl>`/`<chr>` type row.
#
# `max_rows`/`max_cols` default to 10, not 20: a data frame's columns are wide.

## Base data frame ----

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
#' @inheritParams paint_matrix
#' @param max_rows,max_cols Elide the middle of the data frame when it has more
#'                        rows or columns than this. Default: `10`, because a data
#'                        frame's columns are wide.
#'
#' @return
#' `paint_data_frame()` invisibly returns the resolved cell table.
#' `gpaint_data_frame()` returns a `ggplot` object. See `paint_matrix()` for what
#' that object is and is not.
#'
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
    show_indices = c("none", "cell", "row", "column", "all"),
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
    show_types = TRUE,
    show_names = TRUE) {
  force(graph_title)

  if (!is.data.frame(data)) {
    stop("Please double-check the data supplied is of a `data.frame` type.")
  }
  show_indices <- match.arg(show_indices)
  subtle_digits <- match.arg(subtle_digits)
  # A data frame is a grid, so it takes the grid subtitle: rows, columns, class.
  # Three painters, one contract -- a default line that two of them drew and the
  # third did not would be drift of exactly the kind the shared cell table exists
  # to prevent.
  graph_subtitle <- resolve_subtitle(graph_subtitle, dims_subtitle(data))

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
    show_names = show_names,
    show_types = show_types
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

## ggplot2 data frame ----

#' @rdname paint-data-frame
#' @export
#' @examples
#' # ggplot2 graphics ----
#'
#' gpaint_data_frame(head(iris, 5))
#'
#' gpaint_df(head(mtcars, 4))
gpaint_data_frame <- function(
    data,
    show_indices = c("none", "cell", "row", "column", "all"),
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
    show_types = TRUE,
    show_names = TRUE) {
  force(graph_title)
  require_ggplot2()

  if (!is.data.frame(data)) {
    stop("Please double-check the data supplied is of a `data.frame` type.")
  }
  show_indices <- match.arg(show_indices)
  subtle_digits <- match.arg(subtle_digits)
  graph_subtitle <- resolve_subtitle(graph_subtitle, dims_subtitle(data))

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
    show_names = show_names,
    show_types = show_types
  )

  gpaint_skin(prep, graph_title, graph_subtitle)
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
