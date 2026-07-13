# `paint_size()` -- the honest answer to "my plot is too small".
#
# The tempting answer is to resize the device. It was investigated and it is dead:
#
#   1. THERE IS NO RESIZE API. `dev.size()` is a pure accessor; `dev.size<-` does
#      not exist.
#   2. `dev.new()` DOES NOT RESIZE -- it opens a NEW device and makes it current.
#      In a non-interactive session (`Rscript`, `R CMD check`, knitr)
#      `getOption("device")` resolves to `pdf`, so it writes `Rplots.pdf` into the
#      working directory, which CRAN policy forbids.
#   3. WORSE, IT HIJACKS THE USER'S DEVICE. After `dev.new()` everything paintr
#      draws goes to the new device, and the knitr chunk's figure comes out EMPTY
#      -- a total failure in this package's primary medium.
#
# So `paint_size()` computes the answer analytically and hands it back as a number
# for the user to paste into `png()` or a chunk header. It OPENS NO DEVICE and
# READS NO DEVICE: `measure_mono()` is a fake font (Courier's 0.6 em advance
# width), which is exactly why this works in a fresh session with nothing open.
#
# The arithmetic is one division, and it is exact rather than iterative because
# `fit_fontsize()` is LINEAR IN `u`: every term in it is `u * something` -- the
# width fit, the height fit, and the stacked-pair fit alike -- and the only
# non-linearity is the `max_pt` cap, which is lifted here. So measure the fit at
# `u = 1` to get points-per-inch-of-unit, and the unit size that puts the text
# exactly on the legibility floor is `min_pt / that`.

#' The device size a data structure needs
#'
#' How big the graphics device has to be for [paint_matrix()] and friends to draw
#' `data` at the legibility floor. Opens no device and reads no device, so it
#' works in a fresh session with nothing plotted -- which is the whole point, since
#' the situation it exists for is "the device I have is too small".
#'
#' The answer is a lower bound: at exactly this size the text lands on `min_pt`,
#' so round up in practice.
#'
#' @param data A vector, matrix, or data frame.
#' @param ... Passed to the cell builder. Anything the painters accept that
#'   changes the *shape* of the drawing belongs here -- `show_indices`,
#'   `max_rows`, `max_cols`, `show_all`, `layout`, `sigfig`, `max_chars`,
#'   `show_names`, `show_types`.
#' @param min_pt The legibility floor, in points. The returned size is the one
#'   that puts the fitted text exactly here.
#' @param family Font family. Only its metrics matter, and `measure_mono()` is
#'   used for all of them, so this argument currently changes nothing; it is
#'   present so the signature matches the painters'.
#' @param units `"in"`, `"cm"`, or `"px"`.
#' @param dpi Pixels per inch, used only when `units = "px"`.
#'
#' @return A named numeric vector, `c(width = , height = )`. Inches and
#'   centimetres are rounded up to a tenth; pixels are rounded up to a whole
#'   pixel.
#'
#' @examples
#' paint_size(matrix(1:400, nrow = 20), show_all = TRUE)
#'
#' # Paste it straight into a device call:
#' #   s <- paint_size(iris, show_all = TRUE)
#' #   png("iris.png", width = s[["width"]], height = s[["height"]],
#' #       units = "in", res = 96)
#'
#' # Or into a knitr chunk header, in inches.
#' paint_size(iris, show_all = TRUE)
#'
#' @export
paint_size <- function(data, ...,
                       min_pt = 5,
                       family = "mono",
                       units = c("in", "cm", "px"),
                       dpi = 96) {
  units <- match.arg(units)
  if (length(min_pt) != 1L || is.na(min_pt) || !is.numeric(min_pt) || min_pt <= 0) {
    stop("`min_pt` must be a single positive number.")
  }
  if (length(dpi) != 1L || is.na(dpi) || !is.numeric(dpi) || dpi <= 0) {
    stop("`dpi` must be a single positive number.")
  }

  dots <- list(...)
  if (is.null(dots$ellipsis)) {
    dots$ellipsis <- getOption("paintr.ellipsis", "...")
  }
  cells <- do.call(paint_cells, c(list(data = data), dots))

  col_w <- column_widths(cells)
  n_row <- attr(cells, "n_row")

  # `max_pt = Inf` lifts the cap, which is the only thing in `fit_fontsize()` that
  # is not linear in `u`. With it lifted, `fs = k * u` exactly, so one measurement
  # at `u = 1` gives `k` and the answer is a division.
  opts <- paint_opts(
    family = family,
    fontsize = NULL,
    min_pt = min_pt,
    max_pt = Inf
  )
  k <- fit_fontsize(cells, col_w, u = 1, measure = measure_mono(family), opts = opts)
  if (!is.finite(k) || k <= 0) {
    stop("Cannot compute a size for this data: it demands no space.")
  }

  u_need <- min_pt / k
  panel_w <- u_need * sum(col_w)
  panel_h <- u_need * as.double(n_row)

  # The painters always draw a title and a subtitle, and they may draw an elision
  # note, so the device has to be bigger than the panel by however much
  # `render_base()` reserves for them. Asking `base_mai()` is what keeps the two in
  # step -- it is the one place the band heights are written down.
  #
  # The two strings are PLACEHOLDERS, and they can be, because `base_mai()` sizes a
  # band from its point size and reserves it or not according to `has_text()`: what
  # matters is that a band is THERE, not what it says. What must not happen is a
  # band the painter draws and this reservation forgets -- the panel would then be
  # shorter than assumed, `u` smaller, and the recommended size would land the text
  # UNDER the floor it was asked to clear. Over-reserving is safe (the text comes
  # out a shade larger than `min_pt`); under-reserving is a suggestion that does not
  # work. So the subtitle is reserved unconditionally, exactly as the title is,
  # because a painter called with the default `graph_subtitle = NULL` draws one.
  mai <- base_mai(
    graph_title = "Data Object: x",
    graph_subtitle = "Dimensions: x",
    note = attr(cells, "note")
  )
  w_in <- panel_w + mai[[2L]] + mai[[4L]]
  h_in <- panel_h + mai[[1L]] + mai[[3L]]

  out <- switch(
    units,
    "in" = c(w_in, h_in),
    "cm" = c(w_in, h_in) * 2.54,
    "px" = c(w_in, h_in) * dpi
  )
  # Round UP, always. A suggested size that is a hair too small is worse than
  # useless: it is a suggestion that does not work.
  out <- if (units == "px") ceiling(out) else ceiling(out * 10) / 10

  c(width = out[[1L]], height = out[[2L]])
}
