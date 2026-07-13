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
#      `mex` to 1, and `op` carries `fg`, and setting `fg` resets `col` -- all of
#      them sort after the parameter they clobber, so `par(op)` restores the
#      caller's values and then immediately throws them away. Verified: `par(col =
#      "red", cex = 1.3)`, then a bare `op <- par(no.readonly = TRUE); par(op)`
#      round trip, leaves `col` at "black" and `cex` at 1, having touched nothing
#      in between. Same for `par(mex = 1.5)`: it comes back 1.
#
#      And re-applying `cex` and `col` afterwards is STILL not enough, because of
#      `csi` and `mex`. `mai == mar * csi * mex`, and:
#
#        * `csi` is `cex` times a device constant, it is READ-ONLY (so it is not
#          in `op`), and it is LAZY (so `par(cex = op$cex)` does not refresh it);
#        * `mex` IS in `op`, but `par(op)`'s `mfrow` reset silently clears it,
#          exactly as it clears `cex` and `col`.
#
#      So the reset re-derives `mai` from `mar` at `cex = 1, mex = 1`, and unless
#      BOTH are put back before the margins are, `mar` (or `mai`) stays wrong --
#      a margin leak into the caller's next plot, which is bug 6 again in
#      miniature. `restore_par()` reproduces the caller's whole
#      `(cex, csi, mex, mar/mai, oma/omi, pin/plt, mfg, new)` state instead of
#      trusting `par(op)`. Its docs carry the full account.
#
#   6. `mex` IS A SECOND `cex`, AND IT MUST BE PINNED THE SAME WAY. `render_base()`
#      pins `par(cex = 1)` so that a size in points is a size in points. It must
#      equally pin `par(mex = 1)`, because `mex` is the conversion factor between
#      the two units this file speaks: `base_mai()` reserves the bands in INCHES,
#      `draw_bands()` draws into them with `mtext(line =)`, in MARGIN LINES, and
#      one margin line is `csi * mex` inches. Leave `mex` at the caller's value
#      and the two disagree by exactly that factor: at `par(mex = 2)` the note is
#      drawn twice as far below the panel as the band reserved for it, and the
#      device edge cuts it in half. Pinning `mex = 1` makes `csi` the single
#      conversion constant, and it is what makes the picture a function of the
#      DEVICE rather than of the caller's `par()`.

# The note ("# 18 more rows") ink.
note_ink <- "grey40"

# How far below the plot region `mtext()` sets the note, in MARGIN LINES. Shared
# by `draw_bands()` (which draws there) and `base_mai()` (which must reserve for
# it). If these two ever disagree, the note is clipped by the device edge.
#
# A margin line is `par("csi") * par("mex")` inches. `base_mai()` reserves
# `note_line * csi`, so the two agree ONLY at `mex == 1` -- which is why
# `render_base()` pins `par(mex = 1)` alongside `par(cex = 1)`. Measured, before
# the pin: a note-bearing figure on a 700x500 device with the caller at
# `par(mex = 2)` put the note's ink in rows 497-500 of 500, sliced in half by the
# device edge. With the pin it lands in rows 489-495, whole, and it does not move
# when the caller's `mex` does.
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
#' `par(op)` -- **does not restore `par()`**, and it fails in three separate ways.
#' All are measured, all leak into the caller's session, and none is visible
#' unless the test fixture happens to set the parameter that leaks.
#'
#' **1. `par(op)` throws away `cex`, `col` and `mex`.** `op` carries
#' `mfrow`/`mfcol`/`mfg`, and *setting* any of those resets `cex` and `mex` to 1
#' (see `?par`); `op` also carries `fg`, and setting `fg` resets `col` to it. All
#' three clobbers sort *after* the value they destroy, so `par(op)` restores the
#' caller's values and then discards them a few elements later. Verified, with
#' nothing at all in between:
#'
#' ```
#' par(col = "red", cex = 1.3, mex = 1.5)
#' op <- par(no.readonly = TRUE); par(op)
#' par("col")   # "black"
#' par("cex")   # 1
#' par("mex")   # 1
#' ```
#'
#' **2. Re-applying `cex` afterwards is still not enough, because of `csi`.**
#' `csi` -- the height of a margin line, in inches -- is `cex` times a device
#' constant. It is **read-only**, so it is not in `op` and cannot be restored
#' directly, and it is **lazy**: it is recomputed only when a layout parameter is
#' set, never by `par(cex = )` alone. So a bare `par(cex = op$cex)` puts `cex`
#' back *without* refreshing `csi`.
#'
#' **3. And `csi` is only half of it, because `mai == mar * csi * mex`.** The
#' inch-valued margins are re-derived from the line-valued ones at every layout
#' refresh, at whatever `csi` and `mex` are current. Put `csi` back but not `mex`
#' and the refresh runs at `mex = 1`: for a caller at `par(mex = 2)` the shipped
#' `mar = 5.1 4.1 4.1 2.1` comes back as `10.2 8.2 8.2 4.2`. Their next plot still
#' *looks* right -- `mai` is invariant -- but every `par(mar = )` and
#' `mtext(line = )` they write afterwards is at half scale. `csi` and `mex` must
#' both be restored, and both **before** the margins are.
#'
#' **`mar`/`oma` are ground truth; `mai`/`omi` are derived.** Measured: `par(plt =
#' )` refreshes `csi` and then recomputes `mai` from `mar` and `omi` from `oma` --
#' it does *not* read the inch values back. So once `cex`, `csi` and `mex` are
#' right, restoring the plot region restores the margins too, in both units, and
#' an explicit `par(mai = , omi = )` step is pure ceremony. (Ablated: deleting it
#' changes nothing across 26 `par()` fixtures x 3 devices.)
#'
#' **`pin`/`plt` is an independent slot.** Setting `mai` recomputes `pin` and
#' `plt`, but setting `plt` does not recompute `mai` -- which one wins at the next
#' `plot.new()` is decided by whichever was set last. A caller who said
#' `par(pin = c(3, 2))`, or who set `pty = "s"` without redrawing, has a `pin` that
#' does not follow from their `mai`, so `plt` has to be put back last of the
#' layout parameters.
#'
#' **`op$pin` itself is never set.** `pin` is *derived* -- from the device size,
#' `mai`/`mar` and `pty` -- and `op` carries the caller's derived value, not a
#' free parameter. On a device too small for the caller's own margins, that
#' derived value is negative (measured: a 1.8in square device with the default
#' margins gives `pin = c(0.56, -0.04)`), and `par(pin = <negative>)` errors --
#' so a plain `par(op)` throws on the restore, *after* the picture already drew
#' correctly. Dropping `pin` from `op` before step 1 costs nothing: the margins
#' and `pty` it would have been derived from are restored regardless, in steps 1
#' and 4.
#'
#' **And `op$plt` is derived in exactly the same way, so it needs exactly the same
#' guard.** It was long believed that it did not -- `plt` is a *fraction* of the
#' figure region rather than an inch count, so it looks as though it cannot go
#' negative. It can. `plt` is `mai` divided by `fin`, and once the margins exceed
#' the device the numerator goes negative and the fraction follows it. Measured:
#'
#' ```
#' pdf(NULL, 0.4, 6)                      # narrower than the default mar
#' par("plt")                             # 2.05 -0.05 0.17 0.86
#' pdf(NULL, 7, 5); par(cex = 1.7, mar = c(9, 8, 7, 6), mex = 2.2)
#' par("plt")                             # 0.85 0.36 1.35 -0.05
#' ```
#'
#' and `par()` rejects a negative component of `plt` outright:
#' `invalid value specified for graphical parameter "plt"`. (It accepts an
#' *inverted* region happily -- the 1.8in square device gives a `plt` whose y
#' runs backwards and sets fine -- so it is negativity, not inversion, that is
#' the settable/unsettable line, and `settable_plt()` tests exactly that.)
#'
#' This is the `pin` bug, again, one slot over, and it was left behind when `pin`
#' was fixed. It reproduces with no `paint_size()` in sight -- `pdf(NULL, 0.4, 6);
#' paint_matrix(matrix(1:4, 2))` threw on the restore *after* drawing the picture
#' correctly -- and it is what made `paint_size()`'s own recommendation error, since
#' the size it recommended for a vertical vector was narrower than the default
#' margins.
#'
#' The guard is `pin`'s, not a new idea: **a derived value that the device cannot
#' accept is not restored, because it does not have to be.** `plt` follows from the
#' device size, `mar`/`mai`, `csi`, `mex` and `pty`, and every one of those is put
#' back regardless -- steps 1, 2 and 3 -- so the caller's `plt` re-derives itself.
#' Verified: on both devices above, `par("plt")` comes back bit-for-bit without
#' step 4 ever running.
#'
#' The order below reproduces the caller's whole
#' `(cex, csi, mex, mar/mai, oma/omi, pin/plt, mfg, new)` state exactly, rather
#' than hoping `par(op)` will. **Every step is ordered against a measured side
#' effect; this is not alphabetical taste.**
#'
#'   1. `par(op)` -- everything else: `bg`, `pty`, `ps`, `usr`, `mfrow`...
#'   2. set `cex` to `csi0 / csi_unit`, the `cex` at which a refresh *yields* the
#'      caller's `csi`. This is not always `op$cex`, because `csi` is lazy and the
#'      caller's own state can be path-dependent: `par(mar = ...)` followed by
#'      `par(cex = 1.3)` leaves a caller whose `csi` does not match their `cex`,
#'      and the only way to reproduce that is to reproduce the path.
#'   3. set `mex`. This is the refresh -- `csi` becomes `csi0` -- and it is also
#'      the second half of the `mai == mar * csi * mex` coupling. It must come
#'      *before* step 4, because step 4 is what re-derives the margins.
#'   4. set `plt`: the plot region, and with it `mai` from `mar` and `omi` from
#'      `oma`, now at the caller's own `csi` and `mex`.
#'   5. set `mfg`, and only now. Setting `mfg` has to come *after* step 4, not
#'      before it: `par(plt = )` re-derives `fig` and `mfg` from R's internal panel
#'      counter, so an `mfg` restored any earlier is silently thrown away. This is
#'      what puts a caller who is half way through a `2x2` layout back on their own
#'      panel -- `par(op)` alone rewinds them, because it sets `mfrow` *after*
#'      `mfg` and setting `mfrow` resets the panel pointer.
#'   6. set `cex`, `col` and `new` last. `cex` and `col` because steps 1 and 5 both
#'      clobber them; setting `cex` here does *not* refresh `csi`, which is the
#'      whole reason step 2 was needed, and here it is exactly what we want.
#'      `new` because `par(mfg = )` sets it `TRUE` as a side effect, and a stray
#'      `new = TRUE` makes the caller's next plot overdraw instead of starting a
#'      fresh page.
#'
#' **Why `laid_out` exists.** Steps 1 and 5 are the only ones that touch the panel
#' pointer, and on a device with no plot on it yet they cannot be undone: `par(op)`
#' sets `new = TRUE` through its `mfg` element (isolated: `par(op["mfg"])` alone
#' does it; `par(op["mfrow"])` and `par(op["mfcol"])` do not), and on a virgin
#' device `par(new = FALSE)` is silently ignored, so step 6 cannot take it back.
#' `render_base()`'s "device too small" guard errors on exactly such a device.
#' The answer is not to undo more, it is to have done less: on that path the
#' renderer has set nothing but `cex` and `mex`, so steps 1 and 5 have nothing to
#' put back and are skipped. Nothing touches `mfrow`/`mfg`, so `new` never goes
#' `TRUE` in the first place, and a caller half way through a layout keeps their
#' panel because it was never disturbed.
#'
#' @param op The caller's `par(no.readonly = TRUE)`, captured on entry.
#' @param csi0 The caller's `par("csi")`, captured on entry. Not in `op`.
#' @param csi_unit `par("csi")` measured at `cex = 1, mex = 1`, i.e. the device
#'   constant. `NA` when the renderer errored before it could be measured, in
#'   which case the plain restore is used -- still enough to fix the headline bug.
#' @param laid_out Has the renderer changed the layout yet -- i.e. did it get as
#'   far as `par(mai = )`? `FALSE` means it errored on the device-size guard, and
#'   the caller's `mfrow`/`mfg`/`new` must not be touched. See above.
#'
#' @return `invisible(NULL)`, called for its side effect on the device.
#'
#' @keywords internal
#' @noRd
restore_par <- function(op, csi0, csi_unit = NA_real_, laid_out = TRUE) {
  # `plt` IS `pin`'s problem, one slot over, and it has to be settled BEFORE step 1
  # -- not just at step 4 -- because `plt` is an element of `op` and `par(op)` sets
  # it. That is where it actually threw: the guard has to cover both the bulk
  # restore and the explicit one, so it is decided once, here.
  plt_ok <- settable_plt(op$plt)

  if (isTRUE(laid_out)) {
    # `pin` is derived from the device size, `mai`/`mar` and `pty` -- all of
    # which this list restores anyway (step 1 sets `mai`; step 4 below sets
    # `plt`, which re-derives `pin` from it). On a device too small for the
    # caller's own margins the derived value is negative, and `par(pin =
    # <negative>)` errors, so restoring it directly would throw here even
    # though the picture already drew. Let it come back on its own.
    op$pin <- NULL
    # And so is `plt`, on exactly the devices that make `pin` negative -- it is
    # `mai` over `fin`, so once the margins outgrow the device the fraction goes
    # negative too, and `par()` refuses a negative `plt`. Drop it and let it
    # re-derive from the `mar`/`csi`/`mex`/`pty` that steps 1-3 restore anyway.
    if (!plt_ok) {
      op$plt <- NULL
    }
    graphics::par(op)
  }

  if (isTRUE(is.finite(csi_unit)) && csi_unit > 0 &&
      isTRUE(is.finite(csi0)) && csi0 > 0) {
    graphics::par(cex = csi0 / csi_unit)
    graphics::par(mex = op$mex)
    if (plt_ok) {
      graphics::par(plt = op$plt)
    }
  }

  if (isTRUE(laid_out) && length(op$mfg) == 4L) {
    graphics::par(mfg = op$mfg)
  }

  graphics::par(cex = op$cex, col = op$col, new = op$new)
  invisible(NULL)
}

#' Will `par()` accept this `plt`?
#'
#' `par(plt = )` rejects a **negative** component -- and only a negative one. An
#' *inverted* region (`x1 < x0`, or `y1 < y0`) it takes without complaint, which is
#' why a 1.8in square device, whose `plt` runs backwards in y but stays
#' non-negative, has always restored cleanly while a 0.4in-wide one, whose `plt` is
#' `2.05 -0.05 0.17 0.86`, has always thrown.
#'
#' So the test is exactly "no negative component", and it is deliberately not
#' "is this a sane plot region": narrowing it further would stop restoring a `plt`
#' that the device accepts today, which is a regression, not a fix.
#'
#' @param plt A `par("plt")` value.
#'
#' @return `TRUE` if `par(plt = )` will take it.
#'
#' @keywords internal
#' @noRd
settable_plt <- function(plt) {
  length(plt) == 4L && all(is.finite(plt)) && all(plt >= 0)
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
#' @param csi The height of one margin line, in inches -- `par("csi")`, refreshed.
#'   The note is the one band placed in margin lines rather than inches, so this is
#'   what converts it. A margin line is really `csi * mex`; there is no `mex` here
#'   because [render_base()] pins it to 1 before it reads `csi`, which is the only
#'   thing that keeps this reservation and `draw_bands()`'s `mtext(line = )` in the
#'   same unit.
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

#' How wide the chrome's ink actually is, in inches
#'
#' [base_mai()] models the title, the subtitle and the note as **vertical bands**:
#' it sizes each one from its point size and reserves the height, and it never once
#' looks at what the band SAYS. For a margin reservation that is exactly right --
#' the bands run the full width of the figure, whatever that width turns out to be.
#'
#' For [paint_size()], which has to CHOOSE that width, it is exactly wrong, and it
#' shipped a recommendation that errored. A vertical vector is one narrow column,
#' so the panel it needs is about 0.2in wide, and `paint_size(seq_len(30))` duly
#' recommended a device 0.4in across -- narrower than the default `par("mar")`, and
#' a great deal narrower than the string `"Data Object: seq_len(30)"` the painter
#' was about to draw across the top of it. The painter then errored on that very
#' device. The one thing `paint_size()` exists to do is hand back a size that works.
#'
#' So the width is floored on the ink the chrome really needs. The three strings are
#' the REAL ones here, not the placeholders `base_mai()` is content with: a
#' placeholder has a placeholder's width, and width is the whole question.
#'
#' The measure is injected, and in practice it is always `measure_mono()`:
#' `paint_size()` opens no device and reads no device, so there is no device to ask.
#' That is a feature -- see `R/size.R`.
#'
#' @inheritParams base_mai
#' @param measure A measure: a list of closures `w(s, pt)` and `h(s, pt)`.
#'
#' @return A single number: the widest band's ink, in inches. `0` when there is no
#'   chrome at all.
#'
#' @keywords internal
#' @noRd
base_chrome_w <- function(graph_title = NULL, graph_subtitle = NULL, note = NULL,
                          measure = measure_mono(),
                          title_pt = 12, subtitle_pt = 9, note_pt = 8) {
  band <- function(s, pt) {
    if (!has_text(s)) {
      return(0)
    }
    # `draw_bands()` draws element ONE of whatever it is handed -- a `deparse()`
    # that came back as several lines is truncated to its first -- so element one
    # is what has to fit.
    measure$w(as.character(s)[[1L]], pt)[[1L]]
  }
  max(
    band(graph_title, title_pt),
    band(graph_subtitle, subtitle_pt),
    band(note, note_pt),
    0
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

  # `ink$y` is the cell's centre PLUS its `dy_rel` nudge -- `paint_resolve()`
  # folded the two together, so a `[1, 1]` index arrives here already sitting
  # below the value it names. There is deliberately no `kind` test in this file:
  # the day a renderer starts asking what a cell IS is the day the two backends
  # start drawing different pictures.

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
  # is what converts it back into the units `mtext()` speaks. `render_base()` pins
  # `mex = 1`, so this is `csi` and it agrees with `base_mai()`'s `note_line * csi`
  # by construction; the `mex` factor stays in the formula because it is the true
  # one, and a silent 1 is what let the two drift apart in the first place.
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
#' @return `invisible()` of the list from `paint_resolve()`, with extra fields
#'   recording what the device actually did: `usr` (the window that was
#'   established) and `pin` (the panel that was measured), plus the chrome as it
#'   was drawn -- `graph_title`, `graph_subtitle` and `note`. Returning `usr` is
#'   what lets a test prove `xaxs = "i"` was honoured -- with the default `"r"`,
#'   `usr` comes back padded by 4% and no longer matches the letterbox. Returning
#'   the chrome is what lets a test prove the subtitle a painter *resolved* is the
#'   subtitle that was *drawn*, without reading pixels back off a device.
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
  # Both filled in below. They are read from this frame when the `on.exit()`
  # expression is finally evaluated, so registering the handler before computing
  # them is deliberate: if anything below errors first, `restore_par()` sees `NA`
  # and `FALSE` and falls back to the minimal restore, and the caller's `par()`
  # still survives.
  csi_unit <- NA_real_
  # FALSE until the layout is actually changed. While it is FALSE the renderer has
  # set nothing but `cex` and `mex`, and `restore_par()` must NOT reach for
  # `par(op)` or `par(mfg = )` to put those two back: on a device with no plot on
  # it yet -- which is precisely the device the size guard below errors on -- the
  # `mfg` element of `op` forces `new = TRUE`, and `par(new = FALSE)` cannot then
  # take it back. See `restore_par()`.
  laid_out <- FALSE
  on.exit(restore_par(op, csi0, csi_unit, laid_out), add = TRUE)

  # THE TWO PINS. Everything this file measures is in points and inches, and both
  # of these are the factor that would otherwise turn one of those into the other
  # behind our back.
  #
  # `cex`: `strwidth(cex =)` and `text(cex =)` are BOTH multiples of par("cex"),
  # and `measure_base()` converts points to cex as `pt / par("ps")`, which is only
  # a size in points when par("cex") is 1 -- verified: with par(cex = 2),
  # `strwidth("123", cex = 12 / par("ps"))` doubles.
  #
  # `mex`: one margin line is `par("csi") * par("mex")` inches. `base_mai()`
  # reserves the bands in inches and `draw_bands()` fills them with `mtext(line =)`,
  # in margin lines, so any `mex` but 1 makes the band we reserve and the band we
  # draw into different bands -- and the device edge bisects the note. Verified on
  # a 700x500 device with the caller at `par(mex = 2)`: the note's ink landed in
  # rows 497-500 of 500.
  #
  # Both are restored on exit.
  graphics::par(cex = 1)
  graphics::par(mex = 1)

  # `csi` is LAZY: it is `cex` times a device constant, but it is only recomputed
  # when a layout parameter is set (or at `plot.new()`), so it is STALE the instant
  # after `par(cex = 1)` -- it still holds the CALLER's value. `mex` IS a layout
  # parameter, so the pin above is also the refresh, and `par("csi")` is now the
  # margin line that `mtext()` will really use when we draw.
  #
  # Reading `par("csi")` here WITHOUT that refresh is a live bug, not a nicety: a
  # caller who has already drawn at `par(cex = 0.5)` hands us `csi = 0.1` while the
  # draw happens at `csi = 0.2`, `base_mai()` under-reserves the bottom band by
  # exactly that difference, and the device edge bisects the "# N more rows" note --
  # the very clipping this band exists to prevent.
  csi_unit <- graphics::par("csi")

  mai <- base_mai(
    graph_title, graph_subtitle, note,
    title_pt = title_pt, subtitle_pt = subtitle_pt, note_pt = note_pt,
    # A margin line is `csi * mex` inches, and `mex` is pinned to 1, so it is `csi`
    # inches -- and that is the unit `mtext(line =)` speaks. This is the refreshed
    # value: the band we reserve and the band we draw into are now the same band.
    csi = csi_unit
  )
  fin <- graphics::par("fin")
  if (fin[[1L]] - mai[[2L]] - mai[[4L]] <= 0 || fin[[2L]] - mai[[1L]] - mai[[3L]] <= 0) {
    # Note that `laid_out` is still FALSE here. That is load-bearing: this device
    # may well have nothing drawn on it, and a full `par(op)` restore would strand
    # `new` at TRUE on it.
    stop(
      "The graphics device is too small to draw on: it leaves no room for the ",
      "figure once the margins are reserved. Enlarge the device."
    )
  }

  laid_out <- TRUE
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
  # The chrome, exactly as `draw_bands()` was handed it: `NULL` for a band that
  # was not drawn, the string for one that was.
  resolved$graph_title <- graph_title
  resolved$graph_subtitle <- graph_subtitle
  resolved$note <- note
  invisible(resolved)
}
