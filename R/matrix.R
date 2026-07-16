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

# The shared body of both matrix painters. It validates, checks the lane and
# `match.arg` options, resolves the subtitle, builds the highlight mask from either
# spelling, and calls `painter_prep()`. The ONLY things that stay in the twins are
# `require_ggplot2()` (gpaint only) and the choice of renderer.
#
# It deliberately does NOT touch `graph_title`. That default is a promise over
# `substitute(data)`, and the promise must be forced in the PAINTER's own frame or
# every title collapses to "Data Object: data"; keeping it out of here means the
# trap cannot be sprung one frame down. See `painters.R`.
matrix_prep <- function(data,
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
                        show_dimnames,
                        max_name_chars,
                        highlight_rows = NULL,
                        highlight_columns = NULL,
                        highlight_locations = NULL) {
  if (!is.matrix(data)) {
    # BUG 7. This said "`vector` type", copy-pasted from paint_vector().
    stop("Please double-check the data supplied is of a `matrix` type.")
  }
  show_indices <- check_show_indices(show_indices)
  # A lane VECTOR, not `match.arg()`: `dimnames()` is a list with one slot per
  # axis, so `c("row", "column")` -- and rownames-only -- have to be sayable.
  show_dimnames <- check_lanes(
    show_dimnames, c("none", "row", "column", "all"), "show_dimnames"
  )
  subtle_digits <- match.arg(subtle_digits, c("insignificant", "rounded", "none"))
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
    show_dimnames = show_dimnames,
    max_name_chars = max_name_chars
  )

  list(prep = prep, graph_subtitle = graph_subtitle)
}

#' Visualize Data Inside of a Matrix
#'
#' Generate a graph showing the contents of a matrix.
#'
#' `paint_matrix()` draws on the current base graphics device.
#' `gpaint_matrix()` returns a `ggplot` object.
#'
#' @param data            An object that has the class of `matrix`.
#' @param show_indices    Display indices based on location. A character vector,
#'                        so several kinds of index can be asked for at once.
#'                        Values are:
#'                        `"none"`: no indices, `"cell"`: matrix cell indices `[i, j]`,
#'                        `"row"`: row indices `[i, ]` to the left of the matrix,
#'                        `"column"`: column indices `[, j]` above the matrix, and
#'                        `"all"`: row, column, and cell indices together.
#'                        Default: `"none"`.
#'
#'   Each value switches on its own lane, so `c("row", "column")` draws the row
#'   *and* the column indices but no cell indices, and `"all"` is the same as
#'   `c("cell", "row", "column")`. Combining `"none"` with anything else is
#'   contradictory, and the other values win: `c("none", "row")` draws row
#'   indices. An unknown value is an error, not a silent no-op.
#' @param show_dimnames   Which of the matrix's `dimnames()` to draw. A character
#'                        vector, because `dimnames()` is a *list* with one slot
#'                        per axis: `"none"`, `"row"`, `"column"`, or `"all"`
#'                        (the default). `c("row", "column")` is the same as
#'                        `"all"`, and an axis with no names draws no lane.
#'
#'   An index lane the caller asks for **wins** the axis it names: with
#'   `show_indices = "row"` the row lane draws `[1, ]`, not the row names. To see
#'   both a name and an accessor, ask for `show_indices = "cell"` -- the cell index
#'   is drawn *inside* the cell, so it composes with the names rather than
#'   competing for their lane.
#' @param max_name_chars  Longest a *column* name may be drawn before it is
#'                        truncated. Default: `8`. A matrix is one formatting unit,
#'                        so every column of it is as wide as the widest: a long
#'                        column name widens every cell in the picture. Row names
#'                        sit in a gutter of their own, cost the values nothing, and
#'                        are truncated at `max_chars` instead.
#' @param highlight_area  Logical matrix the same shape as `data`, marking the
#'                        cells to fill. A length-one logical is recycled.
#'                        Default: `NULL`, which highlights nothing.
#' @param highlight_color Color to use to fill the background of a cell.
#' @param highlight_rows,highlight_columns,highlight_locations Shorthand for
#'                        `highlight_area`: instead of building a mask, name the
#'                        rows, columns, or cell locations to fill and the mask is
#'                        built for you with [highlight_data()]. So
#'                        `highlight_rows = 1` is exactly
#'                        `highlight_area = highlight_rows(data, 1)`, and the object
#'                        need not be named twice. Give several at once to fill
#'                        their union. Supplying `highlight_area` together with any
#'                        of these is an error. Default: `NULL`.
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
#' @param palette         Colour palette for the drawing. One of `"mint"` (the
#'                        default), `"slate"`, `"warm"`, or `"classic"` (the
#'                        original look). Defaults to the `"paintr.palette"`
#'                        option when unset. A named list of colours is also
#'                        accepted.
#'
#' @return
#' `paint_matrix()` invisibly returns the resolved cell table: a list with the
#' components `cells`, `fontsize`, `floored`, `u`, `x0`, `y0`, `usr`, `pin`,
#' `graph_title`, `graph_subtitle`, and -- only when the drawing elides -- `note`.
#' `graph_title` and `graph_subtitle` are the chrome as it was actually drawn, so
#' `graph_subtitle` is the resolved default rather than the `NULL` that was passed
#' in. `note` holds the "# N more rows/columns" string and is present only when the
#' drawing elides; it is absent otherwise.
#' `gpaint_matrix()` returns a `ggplot` object.
#'
#' @section The ggplot object is a shell:
#' `gpaint_matrix()` returns a real `ggplot` object -- `+ theme()`, `ggsave()`,
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
#' @family painters
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
#' # A dimnamed matrix labels its rows and columns with its names, like print().
#' mat_named = matrix(
#'   c(21, 6, 22.8, 4), nrow = 2, byrow = TRUE,
#'   dimnames = list(c("Mazda", "Datsun"), c("mpg", "cyl"))
#' )
#' paint_matrix(mat_named)
#'
#' # The name above the column, the accessor under the value: both at once.
#' paint_matrix(mat_named, show_indices = "cell")
#'
#' # An index lane wins the axis it names.
#' paint_matrix(mat_named, show_indices = "row")
#'
#' # Or draw no names at all.
#' paint_matrix(mat_named, show_dimnames = "none")
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
    show_indices = "none",
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
    family = "mono",
    palette = NULL,
    show_dimnames = "all",
    max_name_chars = 8L,
    highlight_rows = NULL,
    highlight_columns = NULL,
    highlight_locations = NULL) {
  # `graph_title` is a promise over `substitute(data)`, so it has to be forced
  # here, in the frame whose `data` the user actually named.
  force(graph_title)

  p <- matrix_prep(
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
    show_dimnames = show_dimnames,
    max_name_chars = max_name_chars,
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

## ggplot2 matrix ----

#' @rdname paint-matrix
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
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
    show_indices = "none",
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
    family = "mono",
    palette = NULL,
    show_dimnames = "all",
    max_name_chars = 8L,
    highlight_rows = NULL,
    highlight_columns = NULL,
    highlight_locations = NULL) {
  force(graph_title)
  require_ggplot2()

  p <- matrix_prep(
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
    show_dimnames = show_dimnames,
    max_name_chars = max_name_chars,
    highlight_rows = highlight_rows,
    highlight_columns = highlight_columns,
    highlight_locations = highlight_locations
  )

  gpaint_skin(p$prep, graph_title, p$graph_subtitle)
}
