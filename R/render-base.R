# Tier 3: the base graphics renderer. The ONLY function in the package that
# *sets* `par()`.
#
# Four things here are load-bearing, and every one of them is a bug that has
# shipped:
#
#   1. BUG 6 -- `par()` RESTORATION. The first two lines of `render_base()` save
#      the caller's `par()` and register its restoration with `on.exit()`.
#      Measured live on `main`: after `paint_matrix()`, the user's next
#      `plot(1:10)` inherits `mar = 0.1, 0.1, 2, 0.1`. That is a CRAN bounce.
#
#   2. The title/note bands are reserved with `par(mai =)` -- INCHES -- and never
#      with `par(mar =)` -- LINES. That is what makes `par("pin")` *be* the
#      panel, in the same unit grid reports, so `panel_base()` and `panel_grid()`
#      cannot disagree.
#
#   3. `plot.window(..., asp = 1, xaxs = "i", yaxs = "i")`. `asp = 1` squares the
#      cells (`par(pty = "s")` does NOT -- it squares the *region*, not the
#      *cells*). `xaxs`/`yaxs = "i"` is the silent killer: the default `"r"` pads
#      the range by 4%, which destroys the "1 user unit == 1 cell" invariant and
#      quietly corrupts every downstream measurement. The current code omits it.
#      Verified: with `"i"`, `par("usr")` reproduces `cell_geometry()`'s
#      letterbox exactly -- `usr[1] == -x0/u`, `usr[3] == -y0/u`, and
#      `diff(usr[1:2])/par("pin")[1] == 1/u` on both axes.
#
#   4. TWO-TONE TEXT IS TWO `text()` CALLS. Verified: `text(x = 0.5, labels =
#      c("0.333", "333"), col = c("black", "grey70"))` warns
#      `length(labels) > max(length(x), length(y)); 'labels' truncated to
#      length 1` and draws ONE span. A colour vector is not a mechanism. The only
#      mechanism is one vectorised, left-anchored call per span.
#
# And one that is NOT in the design document, because it is a defect in the
# idiom the design document recommends:
#
#   5. `par(op)` DOES NOT RESTORE `par()`. `op <- par(no.readonly = TRUE)` captures
#      `mfrow`/`mfcol`/`mfg`, and *setting* any of those resets `cex` to 1 AND
#      `col` to "black" -- documented in `?par`, and they sort after both, so
#      `par(op)` restores the caller's values and then immediately throws them
#      away. Verified: `par(col = "red", cex = 1.3)`, then a bare
#      `op <- par(no.readonly = TRUE); par(op)` round trip, leaves `col` at
#      "black" and `cex` at 1, having touched nothing in between.
#
#      And re-applying `cex` afterwards is STILL not enough, because of `csi`: it
#      is `cex` times a device constant, it is what couples `mar` to `mai` and
#      `oma` to `omi`, it is READ-ONLY (so it is not in `op`), and it is LAZY (so
#      `par(cex = op$cex)` does not refresh it). The `mfrow` reset therefore
#      re-derives `mai` from `mar` at `cex = 1`, and it stays wrong -- a margin
#      leak into the caller's next plot, which is bug 6 again in miniature.
#      `restore_par()` reproduces the caller's whole
#      `(cex, csi, mai, omi, pin/plt, mfg, new)` state instead of trusting
#      `par(op)`. Its docs carry the full account.
#
#      `csi`'s laziness bites a second time on the way IN, not just on the way
#      out: see the `par(mar = par("mar"))` refresh in `render_base()`.

# The note ("# 18 more rows") ink.
note_ink <- "grey40"

# How far below the plot region `mtext()` sets the note, in MARGIN LINES. Shared
# by `draw_bands()` (which draws there) and `base_mai()` (which must reserve for
# it). If these two ever disagree, the note is clipped by the device edge.
note_line <- 0.4

# The grey of the insignificant span is `opts$grey`, NOT a constant in this file.
# It has to be one value shared by both backends -- if base and grid disagree
# about it they draw different pictures -- and `paint_opts()` is the one place
# both of them already read. A constant here would be a second source of truth,
# which is exactly the copy-paste drift that put the same broken `ifelse` chain
# into both `matrix.R` and `vector.R`.

# ---------------------------------------------------------------------------
# par() restoration
# ---------------------------------------------------------------------------

#' Put the caller's `par()` back, all of it
#'
#' The universally recommended idiom -- `op <- par(no.readonly = TRUE)`, then
#' `par(op)` -- **does not restore `par()`**, and it fails in two separate ways.
#' Both are measured, both leak into the caller's session, and neither is visible
#' unless the test fixture happens to set the parameter that leaks.
#'
#' **1. `par(op)` throws away `cex` and `col`.** `op` carries `mfrow`/`mfcol`
#' (and `mfg`), and *setting* any of those resets `cex` to 1 and `col` to
#' `"black"` (see `?par`). They sort *after* `cex` and `col` in `op`, so `par(op)`
#' restores the caller's values and then discards them a few elements later.
#' Verified, with nothing in between:
#'
#' ```
#' par(col = "red", cex = 1.3)
#' op <- par(no.readonly = TRUE); par(op)
#' par("col")   # "black"
#' par("cex")   # 1
#' ```
#'
#' **2. Re-applying `cex` afterwards is still not enough, because of `csi`.**
#' `csi` -- the height of a margin line, in inches -- is `cex` times a device
#' constant, and it is what couples `mar` (lines) to `mai` (inches) and `oma` to
#' `omi`. It is **read-only**, so it is not in `op` and cannot be restored
#' directly, and it is **lazy**: it is recomputed only when a layout parameter is
#' set. So the `mfrow` reset above recomputes `mai` from `mar` at `cex = 1`, and a
#' later bare `par(cex = op$cex)` puts `cex` back *without* refreshing `csi` --
#' leaving `mai` (and `omi`/`oma`) silently wrong. That is a margin leak into the
#' caller's next plot: exactly the bug this whole file exists to kill, just
#' narrower.
#'
#' **3. `pin`/`plt` is a third lazy slot.** Setting `mai` recomputes `pin` and
#' `plt`, but setting `pin` does *not* recompute `mai` -- they are independent
#' slots, and which one wins is decided by whichever was set last. A caller who
#' said `par(pin = c(3, 2))`, or who set `pty = "s"` without redrawing, therefore
#' has a `pin` that does not follow from their `mai`, and step 3 below would
#' overwrite it. Verified: setting `plt` leaves `mai`, `mar` and `csi` untouched,
#' so it can be put back afterwards without undoing any of the above.
#'
#' The order below reproduces the caller's whole
#' `(cex, csi, mar/mai, oma/omi, pin/plt, mfg)` state exactly, rather than hoping
#' `par(op)` will. **Every step is ordered against a measured side effect; this is
#' not alphabetical taste.**
#'
#'   1. `par(op)` -- everything else: `bg`, `pty`, `ps`, `usr`, `mfrow`...
#'   2. set `cex` to `csi0 / csi_unit`, the `cex` at which a refresh *yields* the
#'      caller's `csi`. This is not always `op$cex`, because `csi` is lazy and the
#'      caller's own state can be path-dependent: `par(mar = ...)` followed by
#'      `par(cex = 1.3)` leaves a caller whose `csi` does not match their `cex`,
#'      and the only way to reproduce that is to reproduce the path.
#'   3. set `mai` and `omi` -- the inch-valued halves, which are ground truth.
#'      This is the refresh: `csi` becomes `csi0`, and `mar` and `oma` are
#'      re-derived from it back to the caller's values.
#'   4. set `plt`, undoing step 3's recomputation of the plot region.
#'   5. set `mfg`, and only now. Setting `mfg` has to come *after* step 3, not
#'      before it: `par(mai =)` re-derives `fig` and `mfg` from R's internal panel
#'      counter, so an `mfg` restored any earlier is silently thrown away. This is
#'      what puts a caller who is half way through a `2x2` layout back on their own
#'      panel -- `par(op)` alone rewinds them, because it sets `mfrow` *after*
#'      `mfg` and setting `mfrow` resets the panel pointer.
#'   6. set `cex`, `col` and `new` last. `cex` and `col` because steps 1 and 5 both
#'      clobber them; setting `cex` here does *not* refresh `csi`, which is the
#'      whole reason step 2 was needed, and here it is exactly what we want.
#'      `new` because `par(mfg =)` sets it `TRUE` as a side effect, and a stray
#'      `new = TRUE` makes the caller's next plot overdraw instead of starting a
#'      fresh page.
#'
#' @param op The caller's `par(no.readonly = TRUE)`, captured on entry.
#' @param csi0 The caller's `par("csi")`, captured on entry. Not in `op`.
#' @param csi_unit `par("csi")` measured at `cex = 1`, i.e. the device constant.
#'   `NA` when the renderer errored before it could be measured, in which case the
#'   plain restore is used -- still enough to fix the headline bug.
#'
#' @return `invisible(NULL)`, called for its side effect on the device.
#'
#' @keywords internal
#' @noRd
restore_par <- function(op, csi0, csi_unit = NA_real_) {
  graphics::par(op)

  if (isTRUE(is.finite(csi_unit)) && csi_unit > 0 &&
      isTRUE(is.finite(csi0)) && csi0 > 0) {
    graphics::par(cex = csi0 / csi_unit)
    graphics::par(mai = op$mai, omi = op$omi)
    graphics::par(plt = op$plt)
  }

  if (length(op$mfg) == 4L) {
    graphics::par(mfg = op$mfg)
  }

  graphics::par(cex = op$cex, col = op$col, new = op$new)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# the bands
# ---------------------------------------------------------------------------

#' Reserve the title and note bands, in inches
#'
#' Inches, not `mar` lines. A margin given in lines is a multiple of `par("csi")`,
#' which is a function of the device's `pointsize` -- so the same call reserves a
#' different band on `png(pointsize = 8)` than on `pdf(pointsize = 12)`, and
#' `par("pin")` then stops being the panel that `panel_grid()` reports. Giving the
#' band in inches is what makes the two backends measure the same panel.
#'
#' @param graph_title,graph_subtitle,note Text, or `NULL`/`NA` for absent.
#' @param title_pt,subtitle_pt,note_pt Their sizes, in points.
#' @param side_in Left and right margin, in inches.
#' @param pad_in Breathing room between a band and the panel, in inches.
#'
#' @return A length-4 numeric, in `par("mai")` order: bottom, left, top, right.
#'
#' @keywords internal
#' @noRd
base_mai <- function(graph_title = NULL, graph_subtitle = NULL, note = NULL,
                     title_pt = 12, subtitle_pt = 9, note_pt = 8,
                     side_in = 0.1, pad_in = 0.06, csi = 0.2) {
  # A line of text occupies about 1.4 times its point size.
  lead <- function(pt) 1.4 * pt / 72

  top <- 0
  if (has_text(graph_title)) {
    top <- top + lead(title_pt)
  }
  if (has_text(graph_subtitle)) {
    top <- top + lead(subtitle_pt)
  }
  if (top > 0) {
    top <- top + pad_in
  }

  bottom <- 0
  if (has_text(note)) {
    # `mtext()` places the note `note_line` MARGIN LINES below the plot region,
    # and a margin line is `par("csi")` inches -- NOT a function of the note's
    # own cex. Reserving only `lead(note_pt)` therefore under-reserves by exactly
    # that offset and the device edge bisects the note. Verified by rendering:
    # at 8in the "# 16 more rows" caption came out cut in half.
    bottom <- note_line * csi + lead(note_pt) + pad_in
  }

  # A hairline all round, so a cell's border is not clipped in half by the edge
  # of the figure region.
  c(
    max(bottom, pad_in),
    side_in,
    max(top, pad_in),
    side_in
  )
}

#' Is this a piece of text we would actually draw?
#'
#' `NULL`, `NA`, `character(0)` and `""` all mean "no band".
#'
#' @keywords internal
#' @noRd
has_text <- function(x) {
  length(x) >= 1L && !is.na(x[[1L]]) && nzchar(as.character(x)[[1L]])
}

# ---------------------------------------------------------------------------
# the draw
# ---------------------------------------------------------------------------

#' Draw a resolved cell table into the current plot window
#'
#' The pure drawing half of [render_base()]: it assumes `plot.window()` has
#' already been called and it does not touch `par()`. Everything in a resolved
#' cell table is in **inches from the panel's bottom-left**, so the only thing
#' this function does is convert inches to the user units the window happens to
#' be in, and that conversion is one number:
#' `usr_per_in <- diff(par("usr")[1:2]) / par("pin")[1]`.
#'
#' Text is drawn **left-anchored** (`adj = c(0, 0.5)`) at `x + dx_sig` and
#' `x + dx_insig`. Two calls, for the whole plot. This is not an optimisation --
#' it is the only mechanism base graphics has for two-tone text, because a colour
#' vector on a single `text()` call is silently truncated to one label.
#'
#' @param resolved The list returned by `paint_resolve()`.
#' @param opts From `paint_opts()`.
#'
#' @return `invisible(NULL)`, called for its side effect.
#'
#' @keywords internal
#' @noRd
draw_base <- function(resolved, opts) {
  cells <- resolved$cells
  if (nrow(cells) == 0L) {
    return(invisible(NULL))
  }

  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  # One number converts every measurement in the table. `asp = 1` guarantees the
  # y scale is the same, so there is no second factor.
  usr_per_in <- (usr[[2L]] - usr[[1L]]) / pin[[1L]]
  ux <- function(inches) usr[[1L]] + inches * usr_per_in
  uy <- function(inches) usr[[3L]] + inches * usr_per_in

  # Defensive: an `opts` built before `grey` was a knob still renders.
  grey <- if (is.null(opts$grey)) "grey70" else opts$grey

  # -- the rectangles -------------------------------------------------------
  # `rect()` takes NA for "no fill" and NA for "no border", vectorised, so the
  # cells that have neither simply contribute nothing.
  boxed <- cells$kind != "outline" & (!is.na(cells$fill) | !is.na(cells$border))
  if (any(boxed)) {
    b <- cells[boxed, , drop = FALSE]
    graphics::rect(
      xleft = ux(b$xl), ybottom = uy(b$yb),
      xright = ux(b$xr), ytop = uy(b$yt),
      col = b$fill, border = b$border
    )
  }

  # The outline goes on top of the cell borders, and heavier, so the block reads
  # as one object. Its rectangle was rewritten to the whole value block by
  # `paint_resolve()`, so both backends draw the same box.
  out <- cells[cells$kind == "outline", , drop = FALSE]
  if (nrow(out) > 0L) {
    graphics::rect(
      xleft = ux(out$xl), ybottom = uy(out$yb),
      xright = ux(out$xr), ytop = uy(out$yt),
      col = NA, border = out$border, lwd = 2
    )
  }

  # -- the text: exactly two calls ------------------------------------------
  ink <- cells[nzchar(cells$sig) | nzchar(cells$insig), , drop = FALSE]
  if (nrow(ink) == 0L) {
    return(invisible(NULL))
  }

  # `text()`'s cex multiplies par("ps") * par("cex"). `render_base()` pins
  # par(cex = 1) precisely so that this agrees with `measure_base()`, whose
  # `cex = pt / par("ps")` is only a size in points when par("cex") is 1.
  cex <- ink$fontsize / (graphics::par("ps") * graphics::par("cex"))

  graphics::text(
    x = ux(ink$x + ink$dx_sig), y = uy(ink$y),
    labels = ink$sig, col = ink$ink,
    adj = c(0, 0.5), cex = cex, family = opts$family
  )
  # The grey span. Empty `insig` strings draw nothing, which is why this can be
  # one call over every cell rather than a subset.
  graphics::text(
    x = ux(ink$x + ink$dx_insig), y = uy(ink$y),
    labels = ink$insig, col = grey,
    adj = c(0, 0.5), cex = cex, family = opts$family
  )

  invisible(NULL)
}

#' Draw the title, subtitle and note into the reserved bands
#'
#' @inheritParams base_mai
#' @param opts From `paint_opts()`.
#'
#' @return `invisible(NULL)`.
#'
#' @keywords internal
#' @noRd
draw_bands <- function(graph_title, graph_subtitle, note, opts,
                       title_pt = 12, subtitle_pt = 9, note_pt = 8) {
  ps <- graphics::par("ps")
  usr <- graphics::par("usr")
  # One margin line, in inches. `par(mai =)` reserved the band in inches, so this
  # is what converts it back into the units `mtext()` speaks.
  line_in <- graphics::par("csi") * graphics::par("mex")
  lead <- function(pt) 1.4 * pt / 72

  # Left-aligned at the panel's left edge. `usr[1]` is negative whenever the
  # letterbox centred a narrow block, which is exactly where the title belongs.
  at <- usr[[1L]]

  sub_lines <- if (has_text(graph_subtitle)) lead(subtitle_pt) / line_in else 0

  if (has_text(graph_subtitle)) {
    graphics::mtext(
      text = as.character(graph_subtitle)[[1L]], side = 3, line = 0.25,
      at = at, adj = 0, cex = subtitle_pt / ps, family = opts$family
    )
  }
  if (has_text(graph_title)) {
    graphics::mtext(
      text = as.character(graph_title)[[1L]], side = 3, line = 0.25 + sub_lines,
      at = at, adj = 0, cex = title_pt / ps, family = opts$family
    )
  }
  if (has_text(note)) {
    graphics::mtext(
      text = as.character(note)[[1L]], side = 1, line = note_line,
      at = at, adj = 0, cex = note_pt / ps, col = note_ink,
      family = opts$family
    )
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# the renderer
# ---------------------------------------------------------------------------

#' Render a cell table with base graphics
#'
#' The base backend, and the only function in paintr that **sets** `par()`.
#'
#' It has to own the resolve as well as the draw, and the order is forced:
#' `par("pin")` is not the panel until the title and note bands have been
#' reserved with `par(mai =)`, so the panel cannot be measured -- and therefore
#' the font size cannot be fitted -- until after this function has already
#' changed `par()`. Handing `render_base()` an already-resolved table would mean
#' resolving it against the *wrong* panel.
#'
#' @param cells A cell table from `paint_cells()`.
#' @param col_w Column widths in layout units, from `column_widths()`.
#' @param n_row Drawn rows, from `attr(cells, "n_row")`.
#' @param opts From [paint_opts()].
#' @param measure A measure. Defaults to `measure_base(opts$family)`, which is
#'   the right answer; the argument exists so a test can inject a fake font.
#' @param graph_title,graph_subtitle Drawn above the panel. `NULL` reserves no
#'   band.
#' @param note The `"# 18 more rows"` string, drawn below the panel. `NULL`
#'   reserves no band.
#' @param warn_floor Warn when the fitted size lands below `opts$min_pt`? The
#'   painter reads `getOption("paintr.warn_floor")` and threads it down; nothing
#'   below the painter calls `getOption()`.
#' @param title_pt,subtitle_pt,note_pt Sizes of the chrome, in points.
#'
#' @return `invisible()` of the list from `paint_resolve()`, with two extra
#'   fields recording what the device actually did: `usr` (the window that was
#'   established) and `pin` (the panel that was measured). Returning them is what
#'   lets a test prove `xaxs = "i"` was honoured -- with the default `"r"`, `usr`
#'   comes back padded by 4% and no longer matches the letterbox.
#'
#' @keywords internal
#' @noRd
render_base <- function(cells, col_w, n_row,
                        opts = paint_opts(),
                        measure = NULL,
                        graph_title = NULL,
                        graph_subtitle = NULL,
                        note = NULL,
                        warn_floor = TRUE,
                        title_pt = 12,
                        subtitle_pt = 9,
                        note_pt = 8) {
  # BUG 6. These lines are the entire fix, and they must be the first ones:
  # everything below can error, and `on.exit()` is what makes the caller's `par()`
  # survive it.
  #
  # `csi0` is captured alongside `op` because it is NOT IN `op`: `csi` is
  # read-only, so `par(no.readonly = TRUE)` cannot see it, and it is the hidden
  # state that makes this restore hard. See `restore_par()`.
  op <- graphics::par(no.readonly = TRUE)
  csi0 <- graphics::par("csi")
  # Filled in below, once `cex` is pinned. It is read from this frame when the
  # `on.exit()` expression is finally evaluated, so registering the handler before
  # computing it is deliberate: if anything below errors first, `restore_par()`
  # sees `NA` and falls back to the plain restore, and the caller's `par()` still
  # survives.
  csi_unit <- NA_real_
  on.exit(restore_par(op, csi0, csi_unit), add = TRUE)

  # `strwidth(cex =)` and `text(cex =)` are BOTH multiples of par("cex").
  # `measure_base()` converts points to cex as `pt / par("ps")`, which is only a
  # size in points when par("cex") is 1 -- verified: with par(cex = 2),
  # `strwidth("123", cex = 12 / par("ps"))` doubles. Pinning it here is what keeps
  # the measurement and the drawing in the same units. It is restored on exit.
  graphics::par(cex = 1)

  # `csi` is LAZY: it is `cex` times a device constant, but it is only recomputed
  # when a layout parameter is set (or at `plot.new()`), so it is STALE the instant
  # after `par(cex = 1)` -- it still holds the CALLER's value. Setting `mar` to
  # itself is a no-op on `mar` that forces the recompute, and `par("csi")` is then
  # the margin line that `mtext()` will really use when we draw.
  #
  # Reading `par("csi")` here WITHOUT that refresh is a live bug, not a nicety: a
  # caller who has already drawn at `par(cex = 0.5)` hands us `csi = 0.1` while the
  # draw happens at `csi = 0.2`, `base_mai()` under-reserves the bottom band by
  # exactly that difference, and the device edge bisects the "# N more rows" note --
  # the very clipping this band exists to prevent. Both values are restored on exit.
  graphics::par(mar = graphics::par("mar"))
  csi_unit <- graphics::par("csi")

  mai <- base_mai(
    graph_title, graph_subtitle, note,
    title_pt = title_pt, subtitle_pt = subtitle_pt, note_pt = note_pt,
    # A margin line is `csi` inches on THIS device, and that is the unit
    # `mtext(line =)` speaks. This is the refreshed value -- the band we reserve
    # and the band we draw into are now the same band.
    csi = csi_unit
  )
  fin <- graphics::par("fin")
  if (fin[[1L]] - mai[[2L]] - mai[[4L]] <= 0 || fin[[2L]] - mai[[1L]] - mai[[3L]] <= 0) {
    stop(
      "The graphics device is too small to draw on: it leaves no room for the ",
      "figure once the margins are reserved. Enlarge the device."
    )
  }
  graphics::par(mai = mai)

  graphics::plot.new()

  # NOW par("pin") is the panel -- in inches, the same unit `panel_grid()`
  # reports -- and only now can the font size be fitted.
  panel <- panel_base()
  if (is.null(measure)) {
    measure <- measure_base(opts$family)
  }
  resolved <- paint_resolve(cells, col_w, n_row, panel, measure, opts)

  # `asp = 1` squares the CELLS. `par(pty = "s")` squares the REGION, which is a
  # different thing and does not work. `xaxs`/`yaxs = "i"` suppress the default
  # 4% range padding; without them `usr` is not the letterbox and every inch we
  # convert below is wrong.
  graphics::plot.window(
    xlim = c(0, sum(col_w)),
    ylim = c(0, as.double(n_row)),
    asp = 1,
    xaxs = "i",
    yaxs = "i"
  )

  draw_base(resolved, opts)
  draw_bands(
    graph_title, graph_subtitle, note, opts,
    title_pt = title_pt, subtitle_pt = subtitle_pt, note_pt = note_pt
  )

  # `min_pt` is a THRESHOLD, never a clamp. `max(fontsize, min_pt)` would push the
  # text back up until it overlapped -- the exact smear this engine exists to fix.
  # So the text is drawn at its honest size and the user is told, with the real
  # device's numbers.
  #
  # `floor_message()` is owned by `R/warn.R` -- ONE definition, shared by both
  # renderers, because a helper that exists in both backends is a helper that will
  # drift. There is no `warn_floor_once()` here: a base plot is drawn exactly once
  # per call, so there is no redraw to de-duplicate. That is the grid path's
  # problem, not this one's.
  if (isTRUE(warn_floor) && isTRUE(resolved$floored)) {
    warning(
      floor_message(resolved, opts, dev_in = grDevices::dev.size("in")),
      call. = FALSE
    )
  }

  resolved$usr <- graphics::par("usr")
  resolved$pin <- graphics::par("pin")
  invisible(resolved)
}
