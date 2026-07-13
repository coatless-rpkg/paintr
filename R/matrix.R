# The matrix painters. Thin: every decision they used to make themselves now
# belongs to the engine.
#
# What died here:
#
#   * BUG 1 -- the `ifelse(is.finite(...), ..., "Unknown")` chain. A string is
#     not finite, not infinite, not NaN and not NA, so it fell off the end of the
#     chain and every character cell rendered as a red "Unknown". The chain was a
#     partial function pretending to be total. It is gone; `paint_format()` is
#     total by construction, because it dispatches on type instead of guessing.
#   * BUG 2 -- `as.character(value)`, which rendered `1/3` at fifteen digits.
#   * BUG 3 -- `cex = 1.25`, hard-coded against a 1x1 grid.
#   * BUG 6 -- the unrestored `par(mar = ...)`. Only `render_base()` touches
#     `par()` now, and it restores it with `on.exit()`.
#   * BUG 7 -- the type error said "`vector` type", copy-pasted from
#     `paint_vector()`. It says `matrix` now.

## Base matrix ----

#' Visualize Data Inside of a Matrix
#'
#' Generate a graph showing the contents of a matrix.
#'
#' `paint_matrix()` draws on the current base graphics device.
#' `gpaint_matrix()` returns a `ggplot` object.
#'
#' @param data            An object that has the class of `matrix`.
#' @param show_indices    Display indices based on location. Options are:
#'                        `"none"`: no indices, `"cell"`: matrix cell indices `[i, j]`,
#'                        `"row"`: row indices `[i, ]` to the left of the matrix,
#'                        `"column"`: column indices `[, j]` above the matrix, and
#'                        `"all"`: row, column, and cell options. Default: `"none"`.
#' @param highlight_area  Logical matrix the same shape as `data`, marking the
#'                        cells to fill. A length-one logical is recycled.
#'                        Default: `NULL`, which highlights nothing.
#' @param highlight_color Color to use to fill the background of a cell.
#' @param graph_title     Title to appear in the upper left hand corner of the graph.
#' @param graph_subtitle  Subtitle to appear immediately under the graph title.
#'                        `NULL` (the default) describes the data: its dimensions
#'                        and its class. `NA` or `""` draws no subtitle; any other
#'                        string is drawn as given. The default reports the
#'                        dimensions of the data itself, not of the drawing, so an
#'                        elided matrix still reports all of its rows.
#' @param sigfig          Significant digits drawn in black. Digits past the
#'                        `sigfig`-th are drawn in grey; nothing is discarded.
#'                        Must be in `1:15`.
#' @param subtle_digits   Which digits are drawn grey. `"insignificant"` (the
#'                        default) greys everything past the `sigfig`-th
#'                        significant digit; `"rounded"` greys only digits the
#'                        rendering actually lost; `"none"` draws everything black.
#' @param max_chars       Strings longer than this are truncated with an ellipsis.
#' @param max_rows,max_cols Elide the middle of the matrix when it has more rows
#'                        or columns than this. The decision is made on the
#'                        dimensions alone, with no device consulted, so the same
#'                        object elides the same way on every device.
#' @param show_all        Draw every cell, however small the text becomes. Warns
#'                        when the text falls below the legibility floor.
#' @param fontsize        Font size in points. `NULL` (the default) fits the text
#'                        to the device.
#' @param family          Font family. `"mono"` by default.
#'
#' @return
#' `paint_matrix()` invisibly returns the resolved cell table: a list with the
#' components `cells`, `fontsize`, `floored`, `u`, `x0`, `y0`, `usr`, `pin`,
#' `graph_title`, `graph_subtitle` and `note`. The last three are the chrome as it
#' was actually drawn, so `graph_subtitle` is the resolved default rather than the
#' `NULL` that was passed in.
#' `gpaint_matrix()` returns a `ggplot` object.
#'
#' @section The ggplot object is a shell:
#' `gpaint_matrix()` returns a `ggplot` whose only layer is an
#' `annotation_custom()` holding a grid grob, over a meaningless `0..1`
#' coordinate system. It stays a real `ggplot` -- `+ theme()`, `ggsave()` and
#' printing all work -- but `ggplot_build()` sees an empty layer, and adding a
#' geom or a scale to it will not do anything useful. This is not a shortcut: text
#' cannot be measured at build time and `geom_text()` cannot draw two-tone digits,
#' so a custom grob is the only mechanism that exists.
#'
#' @importFrom graphics rect text mtext par plot.new plot.window
#' @rdname paint-matrix
#' @export
#' @examples
#' # Base graphics
#'
#' # Visualize a 3x3
#' mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
#' paint_matrix(mat_3x3)
#'
#' # Show the cell indices
#' paint_matrix(mat_3x3, show_indices = "cell")
#'
#' # Character data renders as the strings it contains.
#' paint_matrix(matrix(letters[1:6], nrow = 2))
#'
#' # Highlight a row
#' mat_4x4 = matrix(seq_len(16), nrow = 4)
#' paint_matrix(
#'   mat_4x4, show_indices = "row",
#'   highlight_area = highlight_rows(mat_4x4, rows = 1)
#' )
#'
#' # Highlight values above 5
#' mat_2x4 = matrix(round(rnorm(16, 5, 2), 2), ncol = 4)
#' paint_matrix(mat_2x4, highlight_area = mat_2x4 > 2)
paint_matrix <- function(
    data,
    show_indices = c("none", "cell", "row", "column", "all"),
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
  # `graph_title` is a promise over `substitute(data)`, so it has to be forced
  # here, in the frame whose `data` the user actually named.
  force(graph_title)

  if (!is.matrix(data)) {
    # BUG 7. This said "`vector` type", copy-pasted from paint_vector().
    stop("Please double-check the data supplied is of a `matrix` type.")
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
    family = family
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

## ggplot2 matrix ----

#' @rdname paint-matrix
#' @export
#' @examples
#' # ggplot2 graphics ----
#'
#' # Visualize a 3x3
#' mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
#' gpaint_matrix(mat_3x3)
#'
#' # View the matrix without any highlighting
#' gpaint_matrix(mat_3x3, highlight_area = FALSE)
#'
#' # Highlight a row
#' mat_2x2 = matrix(c(1, 2, 3, 4), nrow = 2)
#' mat_2x2_mask = matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2)
#' gpaint_matrix(mat_2x2, highlight_area = mat_2x2_mask)
#'
#' # Highlight values above 5
#' mat_3x5 = matrix(round(rnorm(15, 5, 2), 2), ncol = 5)
#' gpaint_matrix(mat_3x5, highlight_area = mat_3x5 > 2)
gpaint_matrix <- function(
    data,
    show_indices = c("none", "cell", "row", "column", "all"),
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

  if (!is.matrix(data)) {
    stop("Please double-check the data supplied is of a `matrix` type.")
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
    family = family
  )

  gpaint_skin(prep, graph_title, graph_subtitle)
}
