# Tier 4: the things that can only be proved by drawing.
#
# BUG 6 is the headline: `render_base()` must leave the caller's `par()` exactly
# as it found it.
#
# THE SUBTLETY THAT MAKES THIS TEST REAL: `par()` is PER DEVICE. Closing the
# device resets `par()` and destroys the evidence, so every assertion below is
# made while the device is still OPEN. A test that draws, calls `dev.off()`, and
# then compares `par()` passes whether or not the bug is fixed.
#
# The other invariants that only a live device can prove are here too: that the
# bands are reserved in INCHES (so `par("pin")` is the panel, in the unit grid
# reports), that `xaxs = "i"` was honoured (so one user unit really is one cell),
# and that two-tone text reaches the device as TWO spans.

local_null_pdf <- function(..., env = parent.frame()) {
  grDevices::pdf(NULL, ...)
  dev <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(dev), envir = env)
  invisible(dev)
}

# The cell table, its column widths and its drawn height -- everything
# `render_base()` needs.
fx <- function(x, ...) {
  cells <- paint_cells(x, ...)
  list(
    cells = cells,
    col_w = column_widths(cells),
    n_row = attr(cells, "n_row"),
    note = attr(cells, "note")
  )
}

draw <- function(f, ...) {
  render_base(f$cells, f$col_w, f$n_row, ...)
}

# The three structures, as the three base painters would hand them over.
fixtures <- function() {
  list(
    matrix = fx(matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3),
                show_indices = "all"),
    vector = fx(letters[1:6], show_indices = "outside"),
    data_frame = fx(data.frame(
      n = c(1.5, 123456.789), s = c("ab", "cdef"), l = c(TRUE, NA),
      stringsAsFactors = FALSE
    ))
  )
}

# ---------------------------------------------------------------------------
# BUG 6 -- par() restoration
# ---------------------------------------------------------------------------

test_that("render_base() restores par() exactly -- measured on a LIVE device", {
  for (nm in names(fixtures())) {
    f <- fixtures()[[nm]]
    local_null_pdf()

    before <- graphics::par(no.readonly = TRUE)
    draw(f, graph_title = "Data Object: x", graph_subtitle = "3 rows x 3 columns")
    after <- graphics::par(no.readonly = TRUE)

    # Compared with the device still open. `expect_equal()`, not
    # `expect_identical()`: restoring `usr` through `par()` round-trips at
    # double precision and can move the last bit.
    expect_equal(after, before, info = nm)
  }
})

test_that("par() survives a user's own non-default settings", {
  local_null_pdf()
  # A caller who has already customised their device. We must give back THEIR
  # values, not the defaults.
  #
  # `col` IS THE MOST IMPORTANT ENTRY IN THIS FIXTURE. `par(op)` sets
  # `mfrow`/`mfcol`, and setting either of those resets BOTH `cex` AND `col` --
  # so a renderer that re-applies only `cex` on exit still hands the caller back
  # a black `col` they never asked for, and this whole file stays green while it
  # happens. A fixture that omits `col` cannot see that bug. `lwd`, `lty` and
  # `pch` are here for the same reason: to make the round trip prove itself over
  # more than the parameters we happen to set.
  graphics::par(
    mar = c(3, 4, 5, 6), cex = 1.3, ps = 9, bg = "ivory", pty = "s",
    col = "red", lwd = 2, lty = 3, pch = 17
  )
  before <- graphics::par(no.readonly = TRUE)

  draw(fixtures()$matrix, graph_title = "t")

  after <- graphics::par(no.readonly = TRUE)
  expect_equal(after, before)
  expect_equal(graphics::par("mar"), c(3, 4, 5, 6))
  expect_equal(graphics::par("cex"), 1.3)
  expect_equal(graphics::par("ps"), 9)
  expect_equal(graphics::par("bg"), "ivory")
  expect_equal(graphics::par("pty"), "s")
  # The one the first fix missed.
  expect_equal(graphics::par("col"), "red")
  expect_equal(graphics::par("lwd"), 2)
  # `par()` normalises the line type to its name, so 3 comes back as "dotted".
  expect_equal(graphics::par("lty"), "dotted")
  expect_equal(graphics::par("pch"), 17)
})

test_that("par(op) alone does NOT restore col -- the same lossy idiom, second casualty", {
  # The exact defect the first fix left behind: it patched `cex` and left `col`
  # leaking. `mfrow`/`mfcol` reset BOTH, so a renderer must re-apply BOTH. This
  # test asserts the idiom is broken (so nobody "simplifies" render_base() back
  # to a bare `par(op)`) and that render_base() is not.
  local_null_pdf()
  graphics::par(col = "red")
  op <- graphics::par(no.readonly = TRUE)
  graphics::par(op) # a bare round trip; nothing at all happens in between
  expect_equal(graphics::par("col"), "black") # ...and red is gone
  expect_equal(op$col, "red")

  # render_base()'s second on.exit() restores `col` as well as `cex`.
  graphics::par(col = "red")
  before <- graphics::par(no.readonly = TRUE)
  draw(fixtures()$matrix)
  expect_equal(graphics::par("col"), "red")
  expect_equal(graphics::par(no.readonly = TRUE), before)
})

# ---------------------------------------------------------------------------
# ...and patching cex + col is STILL not enough: csi is the third casualty
# ---------------------------------------------------------------------------
#
# `csi` -- the height of a margin line, in inches -- is `cex` times a device
# constant. It couples `mar` (lines) to `mai` (inches), and `oma` to `omi`. It is
# READ-ONLY, so `par(no.readonly = TRUE)` cannot capture it, and it is LAZY: it is
# recomputed only when a layout parameter is set, never by `par(cex = )` alone.
#
# So `par(op)`'s `mfrow` reset re-derives `mai` from `mar` at `cex = 1`, and a
# later `par(cex = op$cex)` puts `cex` back WITHOUT refreshing `csi` -- leaving
# `mai` silently wrong. That is a margin leak into the caller's next plot: bug 6
# again, just narrower, and it survives the fix that closes `cex` and `col`.

test_that("cex + col is not sufficient either: the canonical idiom still leaks mai", {
  local_null_pdf()
  # The ORDER matters, and it is the whole trap: setting `mar` AFTER `cex` is what
  # makes the caller's `csi` (and so their `mai`) depend on their `cex`. A fixture
  # that sets `mar` first cannot see this bug -- which is why the one above, which
  # does exactly that, stayed green while `mai` leaked.
  graphics::par(col = "red", cex = 1.3)
  graphics::par(mar = c(3, 4, 5, 6))
  before <- graphics::par(no.readonly = TRUE)

  # The best-known fix, in full: save, restore, then re-apply cex and col.
  op <- graphics::par(no.readonly = TRUE)
  graphics::par(cex = 1) # render_base() must pin this in order to measure text
  graphics::par(mai = c(0.06, 0.1, 0.29, 0.1))
  graphics::plot.new()
  graphics::par(op)
  graphics::par(cex = op$cex, col = op$col)

  expect_equal(graphics::par("cex"), 1.3) # cex: restored
  expect_equal(graphics::par("col"), "red") # col: restored
  # ...and yet the caller's margins are gone anyway.
  expect_false(isTRUE(all.equal(graphics::par("mai"), before$mai)))
  expect_false(isTRUE(all.equal(graphics::par(no.readonly = TRUE), before)))
})

test_that("render_base() restores mai for a caller whose mar was set after their cex", {
  local_null_pdf()
  graphics::par(col = "red", cex = 1.3)
  graphics::par(mar = c(3, 4, 5, 6)) # the ordering the test above proves is lossy
  before <- graphics::par(no.readonly = TRUE)

  draw(fixtures()$matrix, graph_title = "t", note = "# 1 more row")

  expect_equal(graphics::par(no.readonly = TRUE), before)
  expect_equal(graphics::par("mai"), before$mai) # the one the cex+col fix misses
  expect_equal(graphics::par("mar"), c(3, 4, 5, 6))
  expect_equal(graphics::par("col"), "red")
  expect_equal(graphics::par("cex"), 1.3)
})

test_that("render_base() restores oma/omi, pin/plt and a caller's explicit pin", {
  local_null_pdf()
  graphics::par(col = "red", cex = 1.3, oma = c(1, 2, 3, 4))
  before <- graphics::par(no.readonly = TRUE)
  draw(fixtures()$matrix, graph_title = "t")
  expect_equal(graphics::par(no.readonly = TRUE), before)
  expect_equal(graphics::par("oma"), c(1, 2, 3, 4))

  # `pin`/`plt` is an independent slot: setting `mai` recomputes it, but setting
  # it does NOT recompute `mai`. A caller who pinned their plot region by hand
  # must get it back, and restoring `mai` on the way out would otherwise blow it
  # away.
  local_null_pdf()
  graphics::par(col = "red", cex = 1.3, pin = c(3, 2))
  before <- graphics::par(no.readonly = TRUE)
  draw(fixtures()$matrix, graph_title = "t")
  expect_equal(graphics::par(no.readonly = TRUE), before)
  expect_equal(graphics::par("pin"), c(3, 2))
})

test_that("a caller half way through an mfrow layout keeps their panel, and their `new`", {
  local_null_pdf()
  graphics::par(mfrow = c(2, 2))
  plot(1:10) # panel 1 is theirs; the panel pointer has moved on
  graphics::par(col = "red", cex = 1.3)
  before <- graphics::par(no.readonly = TRUE)

  draw(fixtures()$matrix, graph_title = "t")

  # `par(op)` alone rewinds them to panel 1: it sets `mfrow` AFTER `mfg`, and
  # setting `mfrow` resets the panel pointer.
  expect_equal(graphics::par(no.readonly = TRUE), before)
  expect_equal(graphics::par("mfg"), before$mfg)
  expect_equal(graphics::par("fig"), before$fig)
  # `par(mfg =)` sets `new = TRUE` as a side effect. A stray `TRUE` makes the
  # caller's next plot overdraw the current panel instead of starting a fresh one.
  expect_false(graphics::par("new"))
})

# ---------------------------------------------------------------------------
# csi's laziness bites on the way IN as well: the note band
# ---------------------------------------------------------------------------

test_that("the note band is reserved with the csi the draw USES, not the caller's", {
  # `render_base()` pins `cex = 1`, so the margin line `mtext()` finally draws on
  # is the `cex = 1` csi. But `par("csi")` read straight after `par(cex = 1)` is
  # STALE -- csi is lazy, and it still holds the CALLER's value. A caller who has
  # already drawn at `cex = 0.5` therefore hands over `csi = 0.1` while the draw
  # happens at `csi = 0.2`, `base_mai()` under-reserves the bottom band by exactly
  # that gap, and the device edge bisects the "# N more rows" note -- the very
  # clipping the band exists to prevent.
  local_null_pdf(width = 7, height = 5)
  graphics::par(cex = 0.5)
  plot(1:10) # what makes the caller's csi live, and small
  csi_caller <- graphics::par("csi")

  f <- fx(matrix(1:600, nrow = 30, ncol = 20))
  expect_true(has_text(f$note)) # or this test is vacuous
  r <- suppressWarnings(draw(f, note = f$note))

  # Measure the csi the draw really ran at, the way render_base() does.
  graphics::par(cex = 1)
  graphics::par(mar = graphics::par("mar"))
  csi_draw <- graphics::par("csi")
  expect_gt(csi_draw, csi_caller) # non-vacuous: the caller's value IS smaller

  # The panel we got back is the one the correct band implies.
  din <- grDevices::dev.size("in")
  mai_used <- base_mai(NULL, NULL, f$note, csi = csi_draw)
  expect_equal(r$pin[[2L]], din[[2L]] - mai_used[[1L]] - mai_used[[3L]])

  # And the caller's stale csi would have reserved a visibly smaller band.
  mai_stale <- base_mai(NULL, NULL, f$note, csi = csi_caller)
  expect_lt(mai_stale[[1L]], mai_used[[1L]])
})

test_that("par(op) alone does NOT restore cex -- the idiom is lossy, and we fix it", {
  # This is a defect in the save/restore idiom that every graphics package uses,
  # and it is not in the design document. `op` carries `mfrow`/`mfcol`, setting
  # either RESETS cex to 1 (see ?par), and they sort after `cex` -- so `par(op)`
  # restores the caller's cex and then discards it one element later.
  local_null_pdf()
  graphics::par(cex = 1.3)
  op <- graphics::par(no.readonly = TRUE)
  graphics::par(op) # a bare round trip; nothing at all happens in between
  expect_equal(graphics::par("cex"), 1) # ...and cex is gone
  expect_equal(op$cex, 1.3)

  # `render_base()`'s second `on.exit()` is what makes the guarantee real.
  graphics::par(cex = 1.3)
  before <- graphics::par(no.readonly = TRUE)
  draw(fixtures()$matrix)
  expect_equal(graphics::par("cex"), 1.3)
  expect_identical(graphics::par(no.readonly = TRUE), before)
})

test_that("the par() assertion is NOT vacuous: main's code fails it", {
  local_null_pdf()
  before <- graphics::par(no.readonly = TRUE)

  # `matrix.R:71` on main, verbatim: par() set, never restored.
  unrestored <- function() {
    graphics::par(mar = c(0.1, 0.1, 2, 0.1))
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 4), ylim = c(0, 3))
    invisible(NULL)
  }
  unrestored()

  after <- graphics::par(no.readonly = TRUE)
  expect_false(isTRUE(all.equal(after, before)))
  # The exact symptom measured on main: the user's next plot inherits this.
  expect_equal(graphics::par("mar"), c(0.1, 0.1, 2, 0.1))
})

test_that("par() is restored even when render_base() errors", {
  local_null_pdf()
  before <- graphics::par(no.readonly = TRUE)
  f <- fixtures()$matrix

  # `paint_resolve()` rejects this -- and it is called AFTER par(mai =) and
  # plot.new(), so only `on.exit()` can save the caller.
  expect_error(draw(list(cells = f$cells, col_w = f$col_w[1:2], n_row = f$n_row)), "widths")
  expect_error(
    render_base(f$cells, f$col_w[1L], f$n_row),
    "widths"
  )
  expect_equal(graphics::par(no.readonly = TRUE), before)
})

# ---------------------------------------------------------------------------
# the bands are reserved in INCHES, not in `mar` lines
# ---------------------------------------------------------------------------

test_that("par('pin') IS the panel: device size minus the reserved bands", {
  local_null_pdf(width = 7, height = 5)
  f <- fixtures()$matrix
  r <- draw(f, graph_title = "t", graph_subtitle = "s", note = "# 1 more row")

  mai <- base_mai("t", "s", "# 1 more row")
  din <- grDevices::dev.size("in")
  expect_equal(r$pin[[1L]], din[[1L]] - mai[[2L]] - mai[[4L]])
  expect_equal(r$pin[[2L]], din[[2L]] - mai[[1L]] - mai[[3L]])
})

test_that("the band is the same size on any device pointsize -- mar would not be", {
  # This is the whole reason for `par(mai =)`. A margin given in LINES is a
  # multiple of par("csi"), which is a function of the device's pointsize, so
  # `par(mar = c(0.1, 0.1, 2, 0.1))` reserves a different band on
  # pdf(pointsize = 6) than on pdf(pointsize = 18) -- and par("pin") then stops
  # being the panel that panel_grid() reports.
  f <- fixtures()$matrix
  pins <- list()
  for (ps in c(6, 18)) {
    local_null_pdf(width = 7, height = 5, pointsize = ps)
    pins[[as.character(ps)]] <- draw(f, graph_title = "t", graph_subtitle = "s")$pin
    grDevices::dev.off()
  }
  expect_equal(pins[["6"]], pins[["18"]])

  # Non-vacuous: the `mar` route really does move with the pointsize.
  csi <- numeric(0)
  for (ps in c(6, 18)) {
    local_null_pdf(width = 7, height = 5, pointsize = ps)
    graphics::par(mar = c(0.1, 0.1, 2, 0.1))
    csi <- c(csi, graphics::par("pin")[[2L]])
    grDevices::dev.off()
  }
  expect_false(isTRUE(all.equal(csi[[1L]], csi[[2L]])))
})

# ---------------------------------------------------------------------------
# xaxs = "i" -- the silent killer
# ---------------------------------------------------------------------------

test_that("xaxs/yaxs = 'i': par('usr') IS the letterbox, with no 4% padding", {
  local_null_pdf(width = 7, height = 5)
  f <- fixtures()$matrix
  r <- draw(f)

  # `cell_geometry()` computed u/x0/y0 in inches from the panel. If `plot.window()`
  # honoured `xaxs = "i"` then par("usr") is exactly that letterbox expressed in
  # layout units. With the default "r" it is padded by 4% and every inch we
  # convert is wrong.
  expect_equal(r$usr[[1L]], -r$x0 / r$u)
  expect_equal(r$usr[[3L]], -r$y0 / r$u)
  expect_equal(r$usr[[2L]], (r$pin[[1L]] - r$x0) / r$u)
  expect_equal(r$usr[[4L]], (r$pin[[2L]] - r$y0) / r$u)

  # One user unit is one cell, on both axes: that is what `asp = 1` buys, and it
  # is the invariant everything downstream rests on.
  expect_equal((r$usr[[2L]] - r$usr[[1L]]) / r$pin[[1L]], 1 / r$u)
  expect_equal((r$usr[[4L]] - r$usr[[3L]]) / r$pin[[2L]], 1 / r$u)

  # The binding axis is filled exactly, edge to edge -- no padding anywhere.
  bind_x <- isTRUE(all.equal(c(r$usr[[1L]], r$usr[[2L]]), c(0, sum(f$col_w))))
  bind_y <- isTRUE(all.equal(c(r$usr[[3L]], r$usr[[4L]]), c(0, f$n_row)))
  expect_true(bind_x || bind_y)
})

test_that("the xaxs assertion is NOT vacuous: the default 'r' pads by 4% a side", {
  local_null_pdf(width = 7, height = 5)
  f <- fixtures()$matrix
  r <- draw(f)

  # Rebuild render_base()'s window on the same panel, with and without the flag.
  # `mai` first, so par("pin") is the same panel render_base() fitted against.
  graphics::par(cex = 1, mai = base_mai(NULL, NULL, NULL))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, sum(f$col_w)), ylim = c(0, f$n_row),
                        asp = 1, xaxs = "i", yaxs = "i")
  good <- graphics::par("usr")
  pin <- graphics::par("pin")

  # The one line the code on main omits.
  graphics::plot.window(xlim = c(0, sum(f$col_w)), ylim = c(0, f$n_row), asp = 1)
  bad <- graphics::par("usr")

  expect_equal(good, r$usr)

  # 4% a side, so each range comes back 8% too wide -- on BOTH axes, because
  # `asp = 1` locks them together.
  expect_equal(diff(bad[1:2]), 1.08 * diff(good[1:2]))
  expect_equal(diff(bad[3:4]), 1.08 * diff(good[3:4]))

  # And that is the corruption: one user unit is no longer one cell. Every inch
  # converted through this window is 8% wrong, silently.
  expect_equal(diff(good[1:2]) / pin[[1L]], 1 / r$u)
  expect_false(isTRUE(all.equal(diff(bad[1:2]) / pin[[1L]], 1 / r$u)))
})

test_that("a cell drawn from the resolved table is square in device space", {
  local_null_pdf(width = 9, height = 4)
  f <- fx(matrix(1:9, nrow = 3))
  r <- draw(f)
  v <- r$cells[r$cells$kind == "value", ]
  # Inches, from paint_resolve().
  expect_equal(v$xr - v$xl, v$yt - v$yb)
  # And the window agrees, so a square cell is drawn square.
  expect_equal((r$usr[[2L]] - r$usr[[1L]]) / r$pin[[1L]],
               (r$usr[[4L]] - r$usr[[3L]]) / r$pin[[2L]])
})

# ---------------------------------------------------------------------------
# two-tone text
# ---------------------------------------------------------------------------

test_that("drawing emits no warnings -- in particular not text()'s label truncation", {
  local_null_pdf(width = 7, height = 5)
  # `text(x = 0.5, labels = c("0.333", "333"), col = c("black", "grey70"))` warns
  # "length(labels) > max(length(x), length(y)); 'labels' truncated to length 1"
  # and draws ONE span. If render_base() ever tries to two-tone with a colour
  # vector, this is the test that catches it.
  for (f in fixtures()) {
    expect_no_warning(draw(f, graph_title = "t", graph_subtitle = "s"))
  }
  # A 3x3 on a 7x5in device fits comfortably, so the floor warning is not what
  # is being suppressed here.
  expect_false(draw(fixtures()$matrix)$floored)
})

test_that("two-tone digits reach the device as TWO spans, one grey", {
  skip_if_not_installed("svglite")
  svg <- tempfile(fileext = ".svg")
  on.exit(unlink(svg), add = TRUE)

  f <- fx(matrix(c(123456.789, 1 / 3, 100000, 0.5), nrow = 2))
  v <- f$cells[f$cells$kind == "value", ]
  expect_true(any(nzchar(v$insig))) # or this test is vacuous

  svglite::svglite(svg, width = 7, height = 5)
  render_base(f$cells, f$col_w, f$n_row)
  grDevices::dev.off()

  txt <- readLines(svg, warn = FALSE)
  txt <- paste(txt, collapse = "\n")

  # Both inks are present, so the value really was split into two spans.
  grey_hex <- grDevices::rgb(
    t(grDevices::col2rgb(paint_opts()$grey)),
    maxColorValue = 255
  )
  expect_true(grepl(grey_hex, txt, ignore.case = TRUE))
  expect_true(grepl("#000000", txt, ignore.case = TRUE))

  # The black span and the grey span of "123457." are separate <text> elements.
  expect_true(grepl(">123<", txt, fixed = TRUE))
  expect_true(grepl(">457.<", txt, fixed = TRUE))
  # And the whole token was never drawn as one label.
  expect_false(grepl(">123457.<", txt, fixed = TRUE))
})

test_that("the drawn spans butt up: dx_insig is dx_sig + w(sig), measured live", {
  local_null_pdf(width = 7, height = 5)
  f <- fx(matrix(c(123456.789, 1 / 3, 100000, 0.5), nrow = 2))
  r <- draw(f)

  # `render_base()` pins par(cex = 1) so that `measure_base()`'s `pt/par("ps")`
  # really is a size in points. Re-measure the way it did.
  graphics::par(cex = 1)
  graphics::plot.new()
  m <- measure_base(paint_opts()$family)
  cs <- r$cells
  w_sig <- numeric(nrow(cs))
  for (p in unique(cs$fontsize)) {
    k <- which(cs$fontsize == p)
    w_sig[k] <- m$w(cs$sig[k], p)
  }
  expect_equal(cs$dx_insig, cs$dx_sig + w_sig)
})

# ---------------------------------------------------------------------------
# the legibility floor is a THRESHOLD, not a clamp
# ---------------------------------------------------------------------------

test_that("render_base() warns below the floor, and still draws at the honest size", {
  local_null_pdf(width = 3, height = 3)
  f <- fx(matrix(1:600, nrow = 30, ncol = 20), show_all = TRUE)

  expect_warning(r <- draw(f), "legibility floor")
  expect_true(r$floored)
  expect_lt(r$fontsize, paint_opts()$min_pt)
  # The bug this replaces: `max(fontsize, min_pt)` would have produced exactly 5
  # and smeared the digits on top of each other.
  expect_false(isTRUE(all.equal(r$fontsize, paint_opts()$min_pt)))

  # And it is silenceable, which is what the painter's `paintr.warn_floor` option
  # threads down.
  expect_no_warning(draw(f, warn_floor = FALSE))
})

# ---------------------------------------------------------------------------
# the bands themselves
# ---------------------------------------------------------------------------

test_that("base_mai() reserves a band only for the text that exists", {
  none <- base_mai(NULL, NULL, NULL)
  ttl <- base_mai("t", NULL, NULL)
  both <- base_mai("t", "s", NULL)
  noted <- base_mai("t", "s", "# 1 more row")

  expect_length(none, 4L)
  expect_lt(none[[3L]], ttl[[3L]])
  expect_lt(ttl[[3L]], both[[3L]])
  expect_equal(both[[1L]], none[[1L]]) # no note, no bottom band
  expect_gt(noted[[1L]], both[[1L]])
  # Left and right never move.
  expect_equal(none[[2L]], noted[[2L]])
  expect_equal(none[[4L]], noted[[4L]])
  # "" and NA are not text.
  expect_equal(base_mai("", NA_character_, NULL), none)
})

test_that("render_base() draws every structure without error, character included", {
  local_null_pdf(width = 7, height = 5)
  for (data in list(
    1:3,
    c(1.5, 2),
    letters,
    c(TRUE, FALSE, NA),
    factor(c("a", "b")),
    c(NA, NaN, Inf, -Inf),
    c(1, 1e15),
    matrix(letters[1:6], nrow = 2),
    matrix(1:900, nrow = 30, ncol = 30),
    iris,
    data.frame(x = 1:2, y = I(list(1:3, "z")))
  )) {
    f <- fx(data)
    before <- graphics::par(no.readonly = TRUE)
    expect_no_error(draw(f, graph_title = "t", note = f$note))
    expect_equal(graphics::par(no.readonly = TRUE), before)
  }
})
