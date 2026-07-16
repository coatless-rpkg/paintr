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
#' @param ... Any shape-affecting painter argument -- for example
#'   `show_indices`, `summarise`, `show_all`, `layout`, `max_rows`, `max_cols`,
#'   `max_slices`, `sigfig`, `max_chars`, `show_names`, `show_types`,
#'   `show_dimnames`, `max_name_chars`, `name_align`, `type_align`, or a list's
#'   `gap`. These are passed to the cell builder, so the estimate matches what the
#'   painter would draw.
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
#' # How large a device does a 20x20 matrix need, drawn in full?
#' paint_size(matrix(1:400, nrow = 20), show_all = TRUE)
#'
#' # The same question in pixels, for a png() at 96 dpi.
#' paint_size(iris, show_all = TRUE, units = "px")
#'
#' # The answer is in inches by default, so it can be pasted straight into a
#' # device call or a knitr chunk header (fig.width, fig.height).
#' s <- paint_size(iris, show_all = TRUE)
#' s
#' # png("iris.png", width = s[["width"]], height = s[["height"]],
#' #     units = "in", res = 96)
#'
#' @family painters
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

  # The chrome the painters will draw, reconstructed EXACTLY as they build it --
  # `deparse(substitute(data))` for the title, and the same default subtitle their
  # own `resolve_subtitle()` will compute. It is needed because the recommendation
  # has to be wide enough to hold it; see `base_chrome_w()`.
  #
  # `substitute()` is evaluated HERE, in the frame the user named `data` in, for
  # the same reason `graph_title` stays in each painter's own formals: one frame
  # further down and every title deparses to the literal "data".
  graph_title <- paste0("Data Object: ", deparse(substitute(data))[[1L]])

  dots <- list(...)
  if (is.null(dots$ellipsis)) {
    dots$ellipsis <- getOption("paintr.ellipsis", "...")
  }
  cells <- do.call(paint_cells, c(list(data = data), dots))

  # `paint_cells()` has now vetted the structure. A vector gets `paint_vector()`'s
  # subtitle; a matrix and a data frame get the one the grid painters use.
  #
  # A LIST NEEDS ITS OWN ARM, and it is not optional. `dims_subtitle()` calls
  # `nrow()` and `ncol()`, both of which are NULL for a list, and `paste0()` DROPS a
  # NULL instead of erroring -- so a list would reserve its chrome against the string
  # "Dimensions:  rows x  columns", which is both a wrong width and, since the same
  # default is what the painter itself will draw, a wrong picture.
  #
  # AN ARRAY NEEDS ITS OWN ARM FOR THE MIRROR-IMAGE REASON, and it is the more
  # dangerous of the two because it does not look broken. `nrow()` and `ncol()` are
  # DEFINED for an array -- they report the first two extents -- so `Titanic` would
  # reserve its chrome against "Dimensions: 4 rows x 2 columns", a sentence that is
  # merely WRONG rather than obviously malformed, while the painter drew a different,
  # longer one and let it run off the device.
  #
  # The rank test is NOT written out here. It lives inside `array_subtitle()`, which
  # hands a rank-2 array back to `dims_subtitle()` itself -- because this function and
  # the painter must agree about the string, and two copies of a rule are two things to
  # forget to change. `paint_size()` cannot know which painter the caller will reach
  # for, so it must be true that they both say the same thing.
  graph_subtitle <- if (is_paint_vector(data)) {
    vector_subtitle(data)
  } else if (is_paint_list(data)) {
    list_subtitle(data)
  } else if (is_paint_array(data)) {
    array_subtitle(data)
  } else {
    dims_subtitle(data)
  }

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
  # For the HEIGHT the strings could be placeholders, and they used to be, because
  # `base_mai()` sizes a band from its point size and reserves it or not according
  # to `has_text()`: what matters is that a band is THERE, not what it says. What
  # must not happen is a band the painter draws and this reservation forgets -- the
  # panel would then be shorter than assumed, `u` smaller, and the recommended size
  # would land the text UNDER the floor it was asked to clear. Over-reserving is
  # safe (the text comes out a shade larger than `min_pt`); under-reserving is a
  # suggestion that does not work. So the subtitle is reserved unconditionally,
  # exactly as the title is, because a painter called with the default
  # `graph_subtitle = NULL` draws one -- and that is now literally true, since the
  # string below IS the one the painter will resolve.
  #
  # For the WIDTH they could not be placeholders, and that is the bug this
  # reconstruction exists to fix. See `base_chrome_w()`.
  note <- attr(cells, "note")
  mai <- base_mai(
    graph_title = graph_title,
    graph_subtitle = graph_subtitle,
    note = note
  )

  # THE PANEL IS NOT THE ONLY THING ON THE DEVICE. `base_mai()` models the chrome
  # as vertical BANDS -- it reserves their height and never looks at their width --
  # so a width taken from the panel alone is a width that forgot the title, the
  # subtitle and the note entirely. For a grid of numbers that is usually harmless,
  # because the grid is the wide thing; for a VERTICAL VECTOR it is not, because a
  # vector is one narrow column. `paint_size(seq_len(30))` recommended 0.4in across
  # -- narrower than the default `par("mar")` -- and `paint_vector()` then ERRORED
  # on the very device it had just been told to open. The one thing this function
  # exists to do is give back a size that works.
  #
  # So the width is the wider of what the cells need and what the chrome needs.
  # Flooring it is SAFE, and safe in the strong sense that it cannot change the
  # fitted size: `cell_geometry()` takes `u = min(panel_w / w_units, panel_h /
  # h_units)`, so a device made WIDER than the cells asked for simply lets the
  # height bind, and `u` -- and with it the font -- is exactly what it was.
  chrome_w <- base_chrome_w(
    graph_title = graph_title,
    graph_subtitle = graph_subtitle,
    note = note,
    measure = measure_mono(family)
  )

  w_in <- max(panel_w, chrome_w) + mai[[2L]] + mai[[4L]]
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
