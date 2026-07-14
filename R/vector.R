# The vector painters. `gpaint_vector()` is new.
#
# The type check is the interesting line. It was `is.vector(data)`, and
# `is.vector()` is FALSE for anything carrying an attribute other than `names` --
# so `paint_vector(factor("a"))` and `paint_vector(Sys.Date())` both errored out
# on a check that was only ever meant to reject matrices. This is the same family
# of dispatch trap as Bug 5 (`inherits(letters, "vector")` is FALSE, so
# `highlight_data.vector()` never fires for an atomic vector): "vector" in R is
# not the concept its name suggests. See `is_paint_vector()`.

## Base vector ----

#' Visualize Data Inside of a Vector
#'
#' Generate a graph showing the contents of a vector.
#'
#' `paint_vector()` draws on the current base graphics device.
#' `gpaint_vector()` returns a `ggplot` object.
#'
#' @param data            An object that has the class of `vector`. Factors,
#'                        Dates and other classed atomic vectors are accepted too.
#' @param layout          Orientation of the vector. Default: `"vertical"`.
#' @param show_indices    Display data indices either `"inside"` the cell,
#'                        `"outside"` it, or `"none"`. Default: `"none"`.
#'                        Exactly one value: a vector has a single index `[i]`,
#'                        so its placements are mutually exclusive. (A matrix or
#'                        data frame has independent row, column and cell lanes,
#'                        and [paint_matrix()] does take several at once.)
#' @param highlight_area  Logical vector the same length as `data`, marking the
#'                        cells to fill. A length-one logical is recycled.
#'                        Default: `NULL`, which highlights nothing.
#' @param highlight_color Color to use to fill the background of a cell.
#' @param graph_title     Title to appear in the upper left hand corner of the graph.
#' @param graph_subtitle  Subtitle to appear immediately under the graph title.
#'                        `NULL` (the default) describes the data: its length and
#'                        its class. `NA` or `""` draws no subtitle; any other
#'                        string is drawn as given. The default reports the length
#'                        of the data itself, not of the drawing, so an elided
#'                        vector still reports all of its elements.
#' @inheritParams paint_matrix
#' @param max_rows,max_cols Elide the middle of the vector when it is longer than
#'                        this. A vertical vector is elided by `max_rows`, a
#'                        horizontal one by `max_cols`.
#'
#' @return
#' `paint_vector()` invisibly returns the resolved cell table. See [paint_matrix()]
#' for its components.
#' `gpaint_vector()` returns a `ggplot` object.
#'
#' @section The ggplot object is a shell:
#' `gpaint_vector()` returns a real `ggplot` object -- `+ theme()`, `ggsave()`,
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
#' @importFrom graphics rect text mtext par plot.new plot.window
#' @rdname paint-vector
#' @export
#' @examples
#' # Base graphics
#'
#' # Visualize a vector with 5 elements
#' vec_5 <- round(rnorm(5, 0, 4), 2)
#' paint_vector(vec_5)
#'
#' # Character vectors render as the strings they contain.
#' paint_vector(letters[1:5], layout = "horizontal")
#'
#' # Visualize a 6 element vector with indices underneath the data
#' vec_6 <- c(-3, 5, NA, Inf, 2, 1)
#' paint_vector(vec_6, layout = "horizontal", show_indices = "inside")
#'
#' # Highlight the 2nd, 4th, and 6th cell with indices shown outside
#' paint_vector(
#'   vec_6, show_indices = "outside",
#'   highlight_area = highlight_locations(vec_6, c(2, 4, 6))
#' )
paint_vector <- function(
    data,
    layout = c("vertical", "horizontal"),
    show_indices = c("none", "inside", "outside"),
    highlight_area = NULL,
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = NULL,
    sigfig = 3L,
    subtle_digits = c("insignificant", "rounded", "none"),
    max_chars = 12L,
    max_rows = 20L,
    max_cols = 15L,
    show_all = FALSE,
    fontsize = NULL,
    family = "mono") {
  force(graph_title)

  if (!is_paint_vector(data)) {
    stop("Please double-check the data supplied is of a `vector` type.")
  }
  layout <- match.arg(layout)
  show_indices <- match.arg(show_indices)
  subtle_digits <- match.arg(subtle_digits)
  graph_subtitle <- resolve_subtitle(graph_subtitle, vector_subtitle(data))

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
    layout = layout
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

## ggplot2 vector ----

#' @rdname paint-vector
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' # ggplot2 graphics ----
#'
#' gpaint_vector(c(-3, 5, NA, Inf, 2, 1))
#'
#' gpaint_vector(letters[1:5], layout = "horizontal", show_indices = "outside")
gpaint_vector <- function(
    data,
    layout = c("vertical", "horizontal"),
    show_indices = c("none", "inside", "outside"),
    highlight_area = NULL,
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = NULL,
    sigfig = 3L,
    subtle_digits = c("insignificant", "rounded", "none"),
    max_chars = 12L,
    max_rows = 20L,
    max_cols = 15L,
    show_all = FALSE,
    fontsize = NULL,
    family = "mono") {
  force(graph_title)
  require_ggplot2()

  if (!is_paint_vector(data)) {
    stop("Please double-check the data supplied is of a `vector` type.")
  }
  layout <- match.arg(layout)
  show_indices <- match.arg(show_indices)
  subtle_digits <- match.arg(subtle_digits)
  graph_subtitle <- resolve_subtitle(graph_subtitle, vector_subtitle(data))

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
    layout = layout
  )

  gpaint_skin(prep, graph_title, graph_subtitle)
}
