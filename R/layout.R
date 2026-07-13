# Tier 2: layout. Pure GIVEN an injected `panel` and `measure`.
#
# This file is where Bug 3 (hard-coded `cex = 1.25`) dies, and it dies testably:
# `paint_resolve()` is a pure function of the cell table, the panel size, and a
# pair of measuring closures. Hand it `panel_fake()` and `measure_mono()` and it
# needs no graphics device at all -- which is what lets the arithmetic that
# actually matters (did the font shrink when the device shrank? do the two spans
# butt up instead of overlapping? do the decimal points line up?) be asserted as
# plain numbers.
#
# Three rules are load-bearing and each one is a bug that has been shipped:
#
#   1. `min_pt` is a WARNING THRESHOLD, never a clamp. `max(fs, min_pt)` is the
#      exact mechanism that produces overlapping text.
#   2. Offsets are computed by PREFIX MEASUREMENT (`dx_insig = dx_sig + w(sig)`),
#      never by addition of separate widths: letters kern, so
#      `w("AV") + w("AW") != w("AVAW")` (verified: off by 0.111in at 100pt on
#      `pdf()`'s Helvetica).
#   3. Decimal alignment is ANCHORING, not space-padding. Padding tokens to a
#      common `nchar` and trusting `family = "mono"` silently degrades to
#      merely-centred in any other family, with no error and no warning.

# The reference string for text height. Height is content-independent on every
# device we have measured (`strheight("")` and `strheight("Mg")` agree on
# `pdf()`), but measuring a fixed ascender+descender pair makes that a property
# of this code rather than a hope about the device.
height_ref <- "Mg"

# ---------------------------------------------------------------------------
# options
# ---------------------------------------------------------------------------

#' Layout options
#'
#' The knobs `paint_resolve()` reads. Bundled so the seam has one argument
#' instead of six, and so a painter can thread `getOption()` values down as
#' plain data.
#'
#' @param family Font family passed to the measure closures. `"mono"` is a
#'   taste, not a correctness crutch -- decimal alignment works in any family.
#' @param fontsize `NULL` autofits; a number pins the size, in points.
#' @param min_pt The legibility floor. A **warning threshold**: text below it is
#'   drawn at its honest size and `floored` comes back `TRUE`. It is never
#'   clamped up.
#' @param max_pt The largest size autofit will choose. Stops a 1x1 matrix from
#'   rendering at 200pt.
#' @param pad Fraction of a cell left empty. Load-bearing, not decorative: it is
#'   what absorbs `pdf()`'s quantization of font size to integer points (a
#'   requested 3.6pt renders at 4pt, an 11% error).
#' @param ref_pt The size at which widths are measured before being scaled.
#'   Deliberately large and integral, because `pdf()` quantizes: measuring at
#'   1pt would inherit the full quantization error, measuring at 100pt keeps it
#'   under half a percent.
#' @param grey The ink the insignificant digits are drawn in. It lives here
#'   rather than on the cell table because `insig` is only ever non-empty on a
#'   black numeric cell, so one colour serves the whole plot -- and because both
#'   renderers need it.
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
paint_opts <- function(family = "mono",
                       fontsize = NULL,
                       min_pt = 5,
                       max_pt = 24,
                       pad = 0.12,
                       ref_pt = 100,
                       grey = "grey70") {
  if (!is.null(fontsize)) {
    if (length(fontsize) != 1L || is.na(fontsize) || !is.numeric(fontsize) || fontsize <= 0) {
      stop("`fontsize` must be a single positive number, or NULL to autofit.")
    }
    fontsize <- as.double(fontsize)
  }
  if (length(min_pt) != 1L || is.na(min_pt) || min_pt <= 0) {
    stop("`min_pt` must be a single positive number.")
  }
  if (length(max_pt) != 1L || is.na(max_pt) || max_pt < min_pt) {
    stop("`max_pt` must be a single number of at least `min_pt`.")
  }
  if (length(pad) != 1L || is.na(pad) || pad < 0 || pad >= 1) {
    stop("`pad` must be a single number between 0 and 1.")
  }
  if (length(ref_pt) != 1L || is.na(ref_pt) || ref_pt <= 0) {
    stop("`ref_pt` must be a single positive number.")
  }
  list(
    family = as.character(family)[1L],
    fontsize = fontsize,
    min_pt = as.double(min_pt),
    max_pt = as.double(max_pt),
    pad = as.double(pad),
    ref_pt = as.double(ref_pt),
    grey = as.character(grey)[1L]
  )
}

# ---------------------------------------------------------------------------
# panels
# ---------------------------------------------------------------------------

#' The panel, in inches
#'
#' The one fact about the device that layout needs. `panel_base()` reads it from
#' `par("pin")` (which is why `render_base()` reserves its title band with
#' `par(mai =)`, in inches, and not with `par(mar =)`, in lines -- so that
#' `par("pin")` *is* the panel, in the same units grid reports).
#' `panel_grid()` reads it from the viewport, at draw time, which is the only
#' moment a ggplot2 panel is real. `panel_fake()` invents one, which is what
#' makes the whole tier testable with no device at all.
#'
#' @return A list with `w_in` and `h_in`.
#'
#' @keywords internal
#' @noRd
panel_base <- function() {
  pin <- graphics::par("pin")
  list(w_in = as.double(pin[[1L]]), h_in = as.double(pin[[2L]]))
}

#' @rdname panel_base
#' @keywords internal
#' @noRd
panel_grid <- function() {
  list(
    w_in = grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE),
    h_in = grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  )
}

#' @param w_in,h_in Panel size, in inches.
#'
#' @rdname panel_base
#' @keywords internal
#' @noRd
panel_fake <- function(w_in, h_in) {
  if (length(w_in) != 1L || length(h_in) != 1L ||
      is.na(w_in) || is.na(h_in) || w_in <= 0 || h_in <= 0) {
    stop("`w_in` and `h_in` must each be a single positive number.")
  }
  list(w_in = as.double(w_in), h_in = as.double(h_in))
}

# ---------------------------------------------------------------------------
# measures
# ---------------------------------------------------------------------------
#
# A measure is a pair of closures, `w(s, pt)` and `h(s, pt)`, each returning
# INCHES. `s` is a character vector; `pt` is one font size. Both must return a
# numeric vector as long as `s`, and both must return 0 -- not NA, not an error
# -- for the empty string, because `insig` is `""` for most cells and a bare
# `strwidth("")` / `grobWidth("")` is a classic source of NA poisoning.

#' Measure text with base graphics
#'
#' `strwidth()` scales by `cex`, which is a multiple of the *device's* `par("ps")`
#' -- so the conversion from points is `cex = pt / par("ps")`, and never `cex = pt`.
#'
#' @param family Font family.
#'
#' @return A list of two closures, `w(s, pt)` and `h(s, pt)`, in inches.
#'
#' @keywords internal
#' @noRd
measure_base <- function(family = "mono") {
  force(family)
  list(
    w = function(s, pt) {
      n <- length(s)
      if (n == 0L) {
        return(numeric(0))
      }
      out <- numeric(n)
      ok <- !is.na(s) & nzchar(s)
      if (any(ok)) {
        u <- unique(s[ok])
        wu <- graphics::strwidth(
          u,
          units = "inches",
          cex = pt / graphics::par("ps"),
          family = family
        )
        out[ok] <- wu[match(s[ok], u)]
      }
      out
    },
    h = function(s, pt) {
      rep(
        graphics::strheight(
          height_ref,
          units = "inches",
          cex = pt / graphics::par("ps"),
          family = family
        ),
        length(s)
      )
    }
  )
}

#' Measure text with grid
#'
#' The same measurement, through `grid`, for `makeContent()`. Verified to agree
#' with [measure_base()] to machine precision on the same `pdf()` device -- which
#' is what makes "base and ggplot draw the same picture" a testable claim rather
#' than a hope.
#'
#' @inheritParams measure_base
#'
#' @return A list of two closures, `w(s, pt)` and `h(s, pt)`, in inches.
#'
#' @keywords internal
#' @noRd
measure_grid <- function(family = "mono") {
  force(family)
  list(
    w = function(s, pt) {
      n <- length(s)
      if (n == 0L) {
        return(numeric(0))
      }
      out <- numeric(n)
      ok <- !is.na(s) & nzchar(s)
      if (any(ok)) {
        u <- unique(s[ok])
        wu <- vapply(
          u,
          function(z) {
            grid::convertWidth(
              grid::grobWidth(
                grid::textGrob(z, gp = grid::gpar(fontsize = pt, fontfamily = family))
              ),
              "inches",
              valueOnly = TRUE
            )
          },
          numeric(1),
          USE.NAMES = FALSE
        )
        out[ok] <- wu[match(s[ok], u)]
      }
      out
    },
    h = function(s, pt) {
      rep(
        grid::convertHeight(
          grid::grobHeight(
            grid::textGrob(height_ref, gp = grid::gpar(fontsize = pt, fontfamily = family))
          ),
          "inches",
          valueOnly = TRUE
        ),
        length(s)
      )
    }
  )
}

#' Measure text analytically, with no device
#'
#' A **fake font**: every glyph is `char_em` ems wide, every line is `line_em`
#' ems tall. It opens no device, reads no device, and is exactly linear at every
#' size.
#'
#' Two jobs, and only two:
#'
#'   * it is the engine behind `paint_size()`, which must compute a required
#'     canvas without opening a device (opening one would write `Rplots.pdf` into
#'     the working directory under `Rscript`, which CRAN forbids);
#'   * it is the **test double** that makes the whole layout tier assertable with
#'     no device open.
#'
#' **It is never the production measure.** Its honest limit is that it cannot see
#' metric divergence: `pdf()` maps the ASCII hyphen to Helvetica's MINUS glyph at
#' 584/1000 em while cairo uses the real hyphen at 333/1000, and a fake font is
#' blind to all of it.
#'
#' `char_em = 0.6` is not a guess -- it is Courier's advance width, and it
#' reproduces `strwidth(family = "mono")` on `pdf()` exactly.
#'
#' @param family Ignored. Present so the three measures are interchangeable.
#' @param char_em Advance width of one glyph, in ems.
#' @param line_em Height of one line, in ems.
#'
#' @return A list of two closures, `w(s, pt)` and `h(s, pt)`, in inches.
#'
#' @keywords internal
#' @noRd
measure_mono <- function(family = "mono", char_em = 0.6, line_em = 1) {
  force(char_em)
  force(line_em)
  list(
    w = function(s, pt) {
      n <- length(s)
      if (n == 0L) {
        return(numeric(0))
      }
      nc <- nchar(s, type = "chars")
      # nchar(NA) is 2, which would silently give a missing value a width.
      nc[is.na(s)] <- 0L
      nc * char_em * pt / 72
    },
    h = function(s, pt) {
      rep(line_em * pt / 72, length(s))
    }
  )
}

#' Measure a vector of strings that are drawn at different sizes
#'
#' A measure takes one `pt`, but the cell table carries a `size_rel` per cell, so
#' a header at 0.9 and a value at 1 are measured in separate calls. Grouping by
#' the distinct sizes keeps that to a handful of calls instead of one per cell,
#' which matters because `grid::convertWidth()` is not cheap.
#'
#' @param wf A measure's `w` closure.
#' @param s A character vector.
#' @param pt A numeric vector of font sizes, the same length as `s`.
#'
#' @return A numeric vector of inches, the same length as `s`.
#'
#' @keywords internal
#' @noRd
measure_sizes <- function(wf, s, pt) {
  out <- numeric(length(s))
  if (length(s) == 0L) {
    return(out)
  }
  for (p in unique(pt)) {
    k <- which(pt == p)
    out[k] <- wf(s[k], p)
  }
  out
}

# ---------------------------------------------------------------------------
# geometry
# ---------------------------------------------------------------------------

#' The isotropic letterbox
#'
#' One layout unit is one row height, and a column of width `1` is therefore a
#' square cell. `u` is how many inches that unit buys on this panel: the smaller
#' of what the width allows and what the height allows. The slack axis is
#' centred, which is `asp = 1` done in arithmetic -- so base and grid cannot
#' disagree about it, and `coord_fixed()` is never needed.
#'
#' @param col_w Column widths in layout units, from `column_widths()`.
#' @param n_row Drawn rows, from `attr(cells, "n_row")`.
#' @param panel A list with `w_in` and `h_in`.
#'
#' @return A list with `u` (inches per layout unit), `x0` and `y0` (the
#'   letterbox offsets, in inches, from the panel's bottom-left), and
#'   `w_units`/`h_units` (the layout extent).
#'
#' @keywords internal
#' @noRd
cell_geometry <- function(col_w, n_row, panel) {
  w_units <- sum(col_w)
  h_units <- as.double(n_row)
  if (!is.finite(w_units) || w_units <= 0 || !is.finite(h_units) || h_units <= 0) {
    stop("The cell table has no extent to lay out.")
  }
  u <- min(panel$w_in / w_units, panel$h_in / h_units)
  list(
    u = u,
    x0 = (panel$w_in - u * w_units) / 2,
    y0 = (panel$h_in - u * h_units) / 2,
    w_units = w_units,
    h_units = h_units
  )
}

#' What each fitting cell demands, in inches per point
#'
#' Width is exactly linear in font size, so a demand measured once at `ref_pt`
#' divides straight into a per-point figure and `fit_fontsize()` needs no solver.
#'
#' The subtlety that overflows cells if you miss it: for a decimal-aligned
#' formatting unit the thing that must fit is **not** the widest token, it is the
#' decimal-aligned unit width `W = max(w(head)) + max(w(tail))`, which can exceed
#' every individual token. `c(1000, 0.001)` has a widest token of 5 characters but
#' occupies `4 + 4 = 8`. Fitting on the token silently overflows.
#'
#' @param cells A cell table.
#' @param measure A measure.
#' @param ref_pt The size to measure at.
#'
#' @return A numeric vector of inches-per-point, one per cell.
#'
#' @keywords internal
#' @noRd
cell_demand <- function(cells, measure, ref_pt) {
  n <- nrow(cells)
  if (n == 0L) {
    return(numeric(0))
  }
  # Prefix measurement of the drawn extent: the two spans are drawn as two
  # left-anchored calls, so what they occupy is w(sig) + w(insig).
  req <- (measure$w(cells$sig, ref_pt) + measure$w(cells$insig, ref_pt)) / ref_pt

  dec <- cells$align == "decimal" & !is.na(cells$fmt_group)
  if (any(dec)) {
    hw <- measure$w(cells$head, ref_pt) / ref_pt
    tw <- measure$w(cells$tail, ref_pt) / ref_pt
    for (g in unique(cells$fmt_group[dec])) {
      k <- which(dec & cells$fmt_group == g)
      unit_w <- max(hw[k]) + max(tw[k])
      req[k] <- pmax(req[k], unit_w)
    }
  }
  req
}

#' Choose the font size
#'
#' Closed form: one division. Font size is linear in `strwidth()` and text height
#' is content-independent, so the largest size that fits is just the tightest
#' ratio of available space to per-point demand.
#'
#' **`min_pt` is not applied here, and that is the entire point.** Clamping the
#' size *up* to a legibility floor is what produces overlapping text -- it is the
#' precise bug this engine exists to fix. The honest small number is returned;
#' `paint_resolve()` reports it as `floored` and the renderer warns.
#'
#' @param cells A cell table.
#' @param col_w Column widths in layout units.
#' @param u Inches per layout unit, from [cell_geometry()].
#' @param measure A measure.
#' @param opts From [paint_opts()].
#'
#' @return A single font size, in points. Capped above at `opts$max_pt`, never
#'   below.
#'
#' @keywords internal
#' @noRd
fit_fontsize <- function(cells, col_w, u, measure, opts) {
  # Height is content-independent, so one measurement serves every cell.
  h1 <- measure$h(height_ref, opts$ref_pt)[[1L]] / opts$ref_pt

  fitting <- which(cells$fit & (nzchar(cells$sig) | nzchar(cells$insig)))
  if (length(fitting) == 0L) {
    fs <- if (h1 > 0) u * (1 - opts$pad) / h1 else opts$max_pt
    return(min(fs, opts$max_pt))
  }

  req <- cell_demand(cells, measure, opts$ref_pt)

  avail_w <- u * col_w[cells$col] * (1 - opts$pad)
  avail_h <- u * (1 - opts$pad)
  sz <- cells$size_rel

  by_w <- ifelse(req > 0, avail_w / (req * sz), Inf)
  by_h <- if (h1 > 0) avail_h / (h1 * sz) else rep(Inf, nrow(cells))

  fs <- min(pmin(by_w, by_h)[fitting])
  min(fs, opts$max_pt)
}

#' Where each span starts, relative to its cell's centre
#'
#' Returns inches, to be added to the cell's centre x. Text is drawn
#' **left-anchored** at that point (`adj = c(0, 0.5)` in base, `hjust = 0` in
#' grid), which is the only mechanism that gets two-tone text into either system:
#' `text(x = 0.5, labels = c("0.333", "333"), col = c("black", "grey70"))` warns
#' and draws one span, and `geom_text()`'s `colour` is one value per row.
#'
#' `align = "decimal"` is the interesting case. Per formatting unit, with
#' `H = max(w(head))` and `T = max(w(tail))`, token `i` starts at
#' `-(H + T)/2 + (H - w(head_i))`. Its decimal point therefore lands at
#' `-(H + T)/2 + H` -- a constant for the whole unit, in **any** font family. No
#' padding, no monospace dependency.
#'
#' `dx_insig` is `dx_sig + w(sig)`: prefix measurement. Adding `w(head)` and
#' `w(tail)` separately would be wrong wherever the font kerns, and we render
#' `<chr>` cells and column headers.
#'
#' @param cells A cell table.
#' @param col_w Column widths in layout units.
#' @param u Inches per layout unit.
#' @param fontsize The base font size, in points.
#' @param measure A measure.
#' @param opts From [paint_opts()].
#'
#' @return A list with numeric `dx_sig` and `dx_insig`, in inches.
#'
#' @keywords internal
#' @noRd
span_dx <- function(cells, col_w, u, fontsize, measure, opts) {
  n <- nrow(cells)
  if (n == 0L) {
    return(list(dx_sig = numeric(0), dx_insig = numeric(0)))
  }
  pt <- fontsize * cells$size_rel

  w_sig <- measure_sizes(measure$w, cells$sig, pt)
  w_insig <- measure_sizes(measure$w, cells$insig, pt)
  w_head <- measure_sizes(measure$w, cells$head, pt)
  w_tail <- measure_sizes(measure$w, cells$tail, pt)
  w_tok <- w_sig + w_insig

  half <- u * col_w[cells$col] * (1 - opts$pad) / 2

  # Centred is the default; every other alignment overrides it.
  dx <- -w_tok / 2

  left <- which(cells$align == "left")
  dx[left] <- -half[left]

  right <- which(cells$align == "right")
  dx[right] <- half[right] - w_tok[right]

  dec <- cells$align == "decimal" & !is.na(cells$fmt_group)
  if (any(dec)) {
    for (g in unique(cells$fmt_group[dec])) {
      k <- which(dec & cells$fmt_group == g)
      hmax <- max(w_head[k])
      tmax <- max(w_tail[k])
      dx[k] <- -(hmax + tmax) / 2 + (hmax - w_head[k])
    }
  }

  list(dx_sig = dx, dx_insig = dx + w_sig)
}

# ---------------------------------------------------------------------------
# the seam
# ---------------------------------------------------------------------------

#' Resolve a cell table against a panel
#'
#' The composition seam, and the only place a device fact is allowed to enter the
#' pipeline. It is **pure given `panel` and `measure`**: hand it `panel_fake()`
#' and `measure_mono()` and it is fully deterministic with no device open, which
#' is what makes the font-size arithmetic assertable as text rather than as an
#' image diff.
#'
#' `floored` is a **field, not an attribute**. `expect_snapshot()` does not print
#' attributes and `expect_equal()` compares them, so an attribute would be both
#' invisible in a snapshot and a nuisance in a comparison.
#'
#' @param cells A cell table from `paint_cells()`.
#' @param col_w Column widths in layout units, from `column_widths()`.
#' @param n_row Drawn rows, from `attr(cells, "n_row")`.
#' @param panel A list with `w_in` and `h_in`.
#' @param measure A measure: a list of closures `w(s, pt)` and `h(s, pt)`.
#' @param opts From [paint_opts()].
#'
#' @return A list with
#'   \describe{
#'     \item{`cells`}{the cell table, plus the columns `fontsize` (points, the
#'       cell's own size, i.e. the base size times `size_rel`), `dx_sig` and
#'       `dx_insig` (inches from the cell's centre), `x`, `y` (the cell's centre,
#'       in inches from the panel's bottom-left) and `xl`, `xr`, `yb`, `yt` (the
#'       cell's rectangle). The `"outline"` cell's rectangle is the whole value
#'       block, not one cell.}
#'     \item{`fontsize`}{the base font size, in points}
#'     \item{`floored`}{did the fit land below `opts$min_pt`?}
#'     \item{`u`}{inches per layout unit}
#'     \item{`x0`,`y0`}{the letterbox offsets, in inches}
#'   }
#'
#' @keywords internal
#' @noRd
paint_resolve <- function(cells, col_w, n_row, panel, measure, opts = paint_opts()) {
  if (!is.data.frame(cells)) {
    stop("`cells` must be a cell table data frame.")
  }
  if (length(col_w) < max(cells$col)) {
    stop(
      "`col_w` has ", length(col_w), " widths but the cell table draws ",
      max(cells$col), " columns."
    )
  }
  geom <- cell_geometry(col_w, n_row, panel)
  u <- geom$u

  fontsize <- if (is.null(opts$fontsize)) {
    fit_fontsize(cells, col_w, u, measure, opts)
  } else {
    opts$fontsize
  }
  # A threshold, not a clamp. There is deliberately no `max(fontsize, min_pt)`
  # anywhere in this file.
  floored <- isTRUE(fontsize < opts$min_pt)

  d <- span_dx(cells, col_w, u, fontsize, measure, opts)

  edge <- c(0, cumsum(col_w))
  cells$fontsize <- fontsize * cells$size_rel
  cells$dx_sig <- d$dx_sig
  cells$dx_insig <- d$dx_insig
  cells$xl <- geom$x0 + u * edge[cells$col]
  cells$xr <- geom$x0 + u * edge[cells$col + 1L]
  # Row 1 is the top row, so its top edge is at the full height.
  cells$yt <- geom$y0 + u * (n_row - cells$row + 1L)
  cells$yb <- geom$y0 + u * (n_row - cells$row)

  # The outline is one cell-table row carrying the top-left of the value block;
  # its extent is the block's bottom-right. Doing that arithmetic here is what
  # guarantees base and grid draw the same rectangle.
  ob <- outline_box(cells)
  if (!is.null(ob)) {
    k <- which(cells$kind == "outline")
    cells$xl[k] <- geom$x0 + u * edge[ob$col0]
    cells$xr[k] <- geom$x0 + u * edge[ob$col1 + 1L]
    cells$yt[k] <- geom$y0 + u * (n_row - ob$row0 + 1L)
    cells$yb[k] <- geom$y0 + u * (n_row - ob$row1)
  }

  cells$x <- (cells$xl + cells$xr) / 2
  cells$y <- (cells$yb + cells$yt) / 2

  list(
    cells = cells,
    fontsize = fontsize,
    floored = floored,
    u = u,
    x0 = geom$x0,
    y0 = geom$y0
  )
}
