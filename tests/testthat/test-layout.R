# Tier 2. Almost all of this runs with NO GRAPHICS DEVICE AT ALL: `panel_fake()`
# invents a panel and `measure_mono()` is an analytic fake font, so
# `paint_resolve()` is a deterministic function of plain numbers. That is what
# makes the questions that actually matter -- does the font shrink when the
# device shrinks? is the floor a threshold or a clamp? do the two spans butt up
# or overlap? do the decimal points line up in a proportional font? -- assertable
# as text.
#
# The handful of tests that DO open a device open `pdf(NULL)`: full `strwidth()`
# and `par()` support, no file I/O, and it is the device `R CMD check` uses.

# A null pdf device that closes itself when the test finishes.
local_null_pdf <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(dev), envir = env)
  invisible(dev)
}

# The three fixtures every test below draws on.
fx <- function(x, ...) {
  cells <- paint_cells(x, ...)
  list(
    cells = cells,
    col_w = column_widths(cells),
    n_row = attr(cells, "n_row")
  )
}

resolve <- function(f, panel, measure = measure_mono(), opts = paint_opts()) {
  paint_resolve(f$cells, f$col_w, f$n_row, panel, measure, opts)
}

# A grid dense enough that autofit never hits the `max_pt` cap, so font size is a
# strictly increasing function of device size.
dense <- function() fx(matrix(1:600, nrow = 30, ncol = 20), show_all = TRUE)

# ---------------------------------------------------------------------------
# the index INSIDE the cell: fixtures and ink measurement
# ---------------------------------------------------------------------------

# Every shape that draws an index inside the cell it names -- which is the only
# configuration in which two spans share one box.
#
# The sizes are chosen to BREAK, not to pass. At a 7in device with default options
# the old fit budgeted the value the whole row while nudging the index only 0.2 of
# a row below it, so the two inks collided from 5x5 up:
#
#     3x3   +0.096in of clear air     4x4   +0.032in
#     5x5   -0.006in  OVERPRINTED     6x6   -0.032in
#     20x20 -0.017in  OVERPRINTED     paint_vector(1:20) -0.044in
#
# A guard fixtured only at 3x3 -- one of the few sizes that happens to clear --
# stays green on a picture in which the value is stamped straight through the
# index. So every one of these sizes is fixtured, and both `"cell"` and `"all"`.
inside_cases <- list(
  list(matrix(1:9, nrow = 3), show_indices = "cell"),
  list(matrix(1:9, nrow = 3), show_indices = "all"),
  list(matrix(1:25, nrow = 5), show_indices = "cell"),
  list(matrix(1:25, nrow = 5), show_indices = "all"),
  list(matrix(1:36, nrow = 6), show_indices = "cell"),
  list(matrix(1:36, nrow = 6), show_indices = "all"),
  list(matrix(1:400, nrow = 20), show_indices = "cell"),
  list(matrix(1:400, nrow = 20), show_indices = "all"),
  list(matrix(c(1, 1 / 3, 1000, 0.001), nrow = 2), show_indices = "cell"),
  list(1:20, show_indices = "inside"),
  list(1:20, show_indices = "inside", layout = "horizontal")
)

case_label <- function(case) {
  shape <- if (is.matrix(case[[1L]])) {
    paste0(nrow(case[[1L]]), "x", ncol(case[[1L]]))
  } else {
    paste0("vector[", length(case[[1L]]), "]")
  }
  layout <- if (is.null(case$layout)) "vertical" else case$layout
  paste(shape, case$show_indices, layout)
}

# The drawn height of a span, in inches. Text height is content-independent, so
# the size is the only thing that varies.
ink_h <- function(measure, pt) {
  vapply(pt, function(p) measure$h(height_ref, p)[[1L]], numeric(1))
}

# THE INK-COLLISION MEASUREMENT, and the reason the old guards did not catch this.
#
# They asserted ANCHOR geometry -- the index's `y` is below the value's `y`, the
# anchor is inside the box -- and every one of those claims stays TRUE while the
# value's descenders are stamped straight through the index's brackets. What has
# to be asserted is the INK: the value's bottom edge against the index's top edge,
# with real measured text heights.
#
# Returns the gap, in inches, for each (value, index) pair. Positive is clear air.
# Negative is the bug.
ink_gap <- function(r, measure) {
  cs <- r$cells
  val <- cs[cs$kind == "value", , drop = FALSE]
  idx <- cs[cs$kind == "cellindex", , drop = FALSE]
  # Pair by the DATA index, not by row order: a pairing by position would still
  # line up if the two chunks were built out of step.
  k <- match(paste(idx$i, idx$j), paste(val$i, val$j))
  stopifnot(!anyNA(k), nrow(idx) > 0L)
  val <- val[k, , drop = FALSE]
  (val$y - ink_h(measure, val$fontsize) / 2) -
    (idx$y + ink_h(measure, idx$fontsize) / 2)
}

# ---------------------------------------------------------------------------
# panels
# ---------------------------------------------------------------------------

test_that("panel_fake() invents a panel and validates it", {
  expect_equal(panel_fake(7, 5), list(w_in = 7, h_in = 5))
  expect_error(panel_fake(0, 5), "positive")
  expect_error(panel_fake(7, NA), "positive")
  expect_error(panel_fake(c(7, 7), 5), "positive")
})

test_that("panel_base() and panel_grid() report the same panel on one device", {
  local_null_pdf()
  graphics::par(mai = c(0.2, 0.2, 0.5, 0.2))
  graphics::plot.new()

  pb <- panel_base()
  expect_equal(pb$w_in, graphics::par("pin")[[1L]])
  expect_equal(pb$h_in, graphics::par("pin")[[2L]])

  grid::grid.newpage()
  pg <- panel_grid()
  expect_equal(pg$w_in, grDevices::dev.size("in")[[1L]])
  expect_equal(pg$h_in, grDevices::dev.size("in")[[2L]])
})

# ---------------------------------------------------------------------------
# the isotropic letterbox
# ---------------------------------------------------------------------------

test_that("cell_geometry() letterboxes isotropically and centres the slack axis", {
  # 5 layout units wide, 2 tall, on a 10 x 10in panel. Width allows u = 2,
  # height allows u = 5; the smaller wins, and the leftover height is split.
  g <- cell_geometry(rep(1, 5), 2L, panel_fake(10, 10))
  expect_equal(g$u, 2)
  expect_equal(g$x0, 0)
  expect_equal(g$y0, 3)
  expect_equal(g$w_units, 5)
  expect_equal(g$h_units, 2)

  # Height binds instead.
  g2 <- cell_geometry(rep(1, 2), 5L, panel_fake(10, 10))
  expect_equal(g2$u, 2)
  expect_equal(g2$x0, 3)
  expect_equal(g2$y0, 0)
})

test_that("a column of width 1 is a SQUARE cell, on any panel", {
  f <- fx(matrix(1:9, nrow = 3))
  # A 3x3 integer matrix: every column is at the min_w floor of 1 layout unit.
  expect_equal(f$col_w, rep(1, 3))

  for (panel in list(panel_fake(3, 3), panel_fake(9, 2), panel_fake(2, 9))) {
    r <- resolve(f, panel)
    v <- r$cells[r$cells$kind == "value", ]
    expect_equal(v$xr - v$xl, v$yt - v$yb)
  }
})

test_that("`asp = 1` in arithmetic: the drawn block never overflows the panel", {
  f <- dense()
  for (panel in list(panel_fake(3, 3), panel_fake(7, 5), panel_fake(14, 20))) {
    r <- resolve(f, panel)
    expect_gte(r$x0, 0)
    expect_gte(r$y0, 0)
    expect_lte(r$u * sum(f$col_w), panel$w_in + 1e-9)
    expect_lte(r$u * f$n_row, panel$h_in + 1e-9)
    # One of the two axes is exactly filled -- that is what makes it a fit and
    # not merely a shrink.
    filled <- isTRUE(all.equal(r$u * sum(f$col_w), panel$w_in)) ||
      isTRUE(all.equal(r$u * f$n_row, panel$h_in))
    expect_true(filled)
  }
})

# ---------------------------------------------------------------------------
# measures
# ---------------------------------------------------------------------------

test_that("measure_mono() is analytic, needs no device, and guards the empty string", {
  m <- measure_mono()
  # nchar * 0.6 em, in inches.
  expect_equal(m$w("abc", 12), 3 * 0.6 * 12 / 72)
  expect_equal(m$w(c("a", "abcd"), 10), c(1, 4) * 0.6 * 10 / 72)
  expect_equal(m$h("anything", 12), 12 / 72)

  # `insig` is "" for most cells. An unguarded strwidth("")/grobWidth("") is how
  # NA gets into the offsets.
  expect_equal(m$w("", 12), 0)
  expect_equal(m$w(c("a", "", "bb"), 12), c(1, 0, 2) * 0.6 * 12 / 72)
  # nchar(NA) is 2, which would silently give a missing value a width.
  expect_equal(m$w(NA_character_, 12), 0)
  expect_equal(m$w(character(0), 12), numeric(0))
  expect_true(all(is.finite(m$w(c("", "x", NA), 9))))
})

test_that("measure_mono()'s 0.6 em is Courier's real advance width", {
  local_null_pdf()
  graphics::plot.new()
  b <- measure_base("mono")
  fake <- measure_mono()
  s <- c("1", "123.456", "-1.5", "1.00e+15")
  expect_equal(fake$w(s, 100), b$w(s, 100))
})

test_that("measure_base() and measure_grid() guard the empty string too", {
  local_null_pdf()
  graphics::plot.new()
  for (m in list(measure_base("mono"), measure_grid("mono"),
                 measure_base("sans"), measure_grid("sans"))) {
    expect_equal(m$w("", 12), 0)
    expect_equal(m$w(character(0), 12), numeric(0))
    expect_equal(m$w(NA_character_, 12), 0)
    expect_true(all(is.finite(m$w(c("", "x", NA), 12))))
    # Height is content-independent, including for "".
    expect_equal(m$h("", 12), m$h("Mg", 12))
    expect_gt(m$h("x", 12), 0)
  }
})

test_that("measure_base() and measure_grid() agree on the same device", {
  local_null_pdf()
  graphics::plot.new()
  s <- c("1", "123.456", "-1.5", "Sepal.Length", "<dbl>", "[10, ]", "NA", "1.00e+15", "")
  for (family in c("mono", "sans", "serif")) {
    b <- measure_base(family)
    g <- measure_grid(family)
    for (pt in c(5, 10, 100)) {
      expect_equal(b$w(s, pt), g$w(s, pt), tolerance = 1e-10)
      expect_equal(b$h(s, pt), g$h(s, pt), tolerance = 1e-10)
    }
  }
})

test_that("prefix measurement is not addition: letters kern", {
  # This is why `dx_insig = dx_sig + w(sig)` and never `w(head) + w(tail)`.
  local_null_pdf()
  graphics::plot.new()
  m <- measure_base("sans")
  expect_false(isTRUE(all.equal(
    m$w("AV", 100) + m$w("AW", 100),
    m$w("AVAW", 100)
  )))
})

# ---------------------------------------------------------------------------
# the font size -- the tier that kills bug 3
# ---------------------------------------------------------------------------

test_that("font size is MONOTONE in device size", {
  f <- dense()
  sizes <- vapply(
    c(3, 7, 14),
    function(s) resolve(f, panel_fake(s, s))$fontsize,
    numeric(1)
  )
  expect_true(all(diff(sizes) > 0))
  # Linear, in fact: the letterbox scales u linearly and width is linear in pt.
  expect_equal(sizes[2] / sizes[1], 7 / 3)
  expect_equal(sizes[3] / sizes[2], 14 / 7)
})

test_that("min_pt is a WARNING THRESHOLD and never a clamp", {
  opts <- paint_opts()

  # A 30x20 crammed onto a 3in device cannot be legible, and the engine says so
  # honestly instead of clamping the size up and smearing the digits.
  small <- resolve(dense(), panel_fake(3, 3), opts = opts)
  expect_lt(small$fontsize, opts$min_pt)
  expect_true(small$floored)
  # The specific bug: `max(fs, min_pt)` would have produced exactly 5.
  expect_false(isTRUE(all.equal(small$fontsize, opts$min_pt)))

  # A 60x40 does not fit at 7in either.
  huge <- resolve(
    fx(matrix(1:2400, nrow = 60, ncol = 40), show_all = TRUE),
    panel_fake(7, 7),
    opts = opts
  )
  expect_lt(huge$fontsize, opts$min_pt)
  expect_true(huge$floored)

  # And the honest size still fits the cell it was measured against -- which a
  # clamped size would not.
  cs <- small$cells[small$cells$kind == "value", ]
  w_tok <- measure_mono()$w(paste0(cs$sig, cs$insig), cs$fontsize[[1L]])
  expect_true(all(w_tok <= (cs$xr - cs$xl) + 1e-9))
})

test_that("floored is a FIELD, not an attribute", {
  r <- resolve(dense(), panel_fake(3, 3))
  expect_true("floored" %in% names(r))
  expect_type(r$floored, "logical")
  expect_length(r$floored, 1L)
  expect_null(attr(r$cells, "floored"))
  expect_null(attr(r, "floored"))

  big <- resolve(dense(), panel_fake(14, 14))
  expect_false(big$floored)
  expect_gte(big$fontsize, paint_opts()$min_pt)
})

test_that("autofit is capped above but a pinned fontsize is obeyed exactly", {
  f <- fx(matrix(1:4, nrow = 2))
  # A 2x2 on 14in would otherwise render at a comical size.
  expect_equal(resolve(f, panel_fake(14, 14))$fontsize, paint_opts()$max_pt)
  expect_equal(
    resolve(f, panel_fake(14, 14), opts = paint_opts(max_pt = 40))$fontsize,
    40
  )

  pinned <- resolve(f, panel_fake(7, 7), opts = paint_opts(fontsize = 9))
  expect_equal(pinned$fontsize, 9)
  # size_rel still scales it per cell.
  expect_equal(
    pinned$cells$fontsize,
    9 * pinned$cells$size_rel
  )

  # Pinning below the floor is honest about it rather than silently overriding.
  expect_true(resolve(f, panel_fake(7, 7), opts = paint_opts(fontsize = 3))$floored)
})

test_that("every fitting cell's text actually fits its cell", {
  for (data in list(
    matrix(1:600, nrow = 30, ncol = 20),
    matrix(c(1, 1 / 3, 1000, 0.001), nrow = 2),
    data.frame(a = c(1.5, 200000), b = c("hello", "a much longer string"),
               c = c(TRUE, FALSE), stringsAsFactors = FALSE),
    letters
  )) {
    f <- fx(data)
    r <- resolve(f, panel_fake(7, 5))
    cs <- r$cells[r$cells$fit & nzchar(paste0(r$cells$sig, r$cells$insig)), ]
    m <- measure_mono()
    w_tok <- m$w(cs$sig, 1) * cs$fontsize + m$w(cs$insig, 1) * cs$fontsize
    expect_true(all(w_tok <= (cs$xr - cs$xl) + 1e-9))
    h_tok <- m$h(cs$sig, 1) * cs$fontsize
    expect_true(all(h_tok <= (cs$yt - cs$yb) + 1e-9))
  }
})

test_that("fitting is on the decimal-aligned UNIT width, not the widest token", {
  # c(1, 1/3, 1000, 0.001) renders "1", "0.333", "1000", "0.001": the widest
  # token is 5 characters, but decimal-anchored the unit occupies
  # max(head) + max(tail) = nchar("1000") + nchar(".333") = 8. Fitting on the
  # token silently overflows.
  f <- fx(matrix(rep(c(1, 1 / 3, 1000, 0.001), 25), nrow = 10, ncol = 10))
  opts <- paint_opts()
  r <- resolve(f, panel_fake(7, 7), opts = opts)
  m <- measure_mono()
  v <- r$cells[r$cells$kind == "value", ]

  fs <- r$fontsize
  expect_lt(fs, opts$max_pt) # not capped, so the fit is genuinely binding

  unit_w <- max(m$w(v$head, fs)) + max(m$w(v$tail, fs))
  max_tok <- max(m$w(paste0(v$sig, v$insig), fs))
  cell_w <- (v$xr - v$xl)[[1L]]

  expect_gt(unit_w, max_tok) # the trap is real for this data
  expect_lte(unit_w, cell_w + 1e-9) # and we did not fall into it

  # The counterfactual: an engine that anchors on the token instead of the
  # decimal point is exactly one whose head IS the token and whose tail is empty.
  naive <- f$cells
  naive$head <- paste0(naive$sig, naive$insig)
  naive$tail <- ""
  fs_naive <- fit_fontsize(naive, f$col_w, r$u, m, opts)
  expect_gt(fs_naive, fs)
  expect_lt(fs_naive, opts$max_pt)

  # At that size the real, decimal-aligned block runs 41% outside its cell.
  unit_naive <- max(m$w(v$head, fs_naive)) + max(m$w(v$tail, fs_naive))
  expect_gt(unit_naive, cell_w)
})

# ---------------------------------------------------------------------------
# two-tone spans
# ---------------------------------------------------------------------------

test_that("dx_insig == dx_sig + w(sig): the spans butt up and never overlap", {
  for (measure in list(measure_mono())) {
    f <- fx(matrix(c(123456.789, 1 / 3, 100000, 0.5), nrow = 2))
    r <- resolve(f, panel_fake(7, 7), measure = measure)
    cs <- r$cells
    w_sig <- numeric(nrow(cs))
    for (p in unique(cs$fontsize)) {
      k <- which(cs$fontsize == p)
      w_sig[k] <- measure$w(cs$sig[k], p)
    }
    expect_equal(cs$dx_insig, cs$dx_sig + w_sig)
    # Never a negative gap (an overlap), never a positive one (a hole).
    expect_true(all(cs$dx_insig - cs$dx_sig >= -1e-12))
  }
})

test_that("dx_insig == dx_sig + w(sig) with a real device measure, in sans", {
  local_null_pdf()
  graphics::plot.new()
  measure <- measure_base("sans")
  f <- fx(matrix(c(123456.789, 1 / 3, 100000, 0.5), nrow = 2))
  r <- resolve(f, panel_fake(7, 7), measure = measure)
  cs <- r$cells
  w_sig <- numeric(nrow(cs))
  for (p in unique(cs$fontsize)) {
    k <- which(cs$fontsize == p)
    w_sig[k] <- measure$w(cs$sig[k], p)
  }
  expect_equal(cs$dx_insig, cs$dx_sig + w_sig)

  # The greyed digits really are present and really are non-empty here, or the
  # test above is vacuous.
  v <- cs[cs$kind == "value", ]
  expect_true(any(nzchar(v$insig)))
})

test_that("in scientific mode insig is empty, so the grey span draws nothing", {
  f <- fx(c(1, 1e15))
  v <- f$cells[f$cells$kind == "value", ]
  expect_true(all(v$insig == ""))
  r <- resolve(f, panel_fake(7, 7))
  vr <- r$cells[r$cells$kind == "value", ]
  # An empty span sits exactly at the end of the black one and has zero width.
  expect_equal(vr$dx_insig - vr$dx_sig, measure_mono()$w(vr$sig, vr$fontsize[[1L]]))
})

# ---------------------------------------------------------------------------
# decimal anchoring -- the one that must work in ANY family
# ---------------------------------------------------------------------------

decimal_x <- function(r, measure) {
  v <- r$cells[r$cells$align == "decimal" & !is.na(r$cells$fmt_group), ]
  v$dec_x <- v$x + v$dx_sig + measure$w(v$head, v$fontsize[[1L]])
  v
}

test_that("every decimal point in a formatting unit lands on the same x -- MONO", {
  measure <- measure_mono()
  f <- fx(matrix(c(1, 1 / 3, 1000, 0.001, -22.5, 7), nrow = 2))
  r <- resolve(f, panel_fake(7, 7), measure = measure)
  v <- decimal_x(r, measure)

  # Within a drawn column, every decimal point is at one x.
  for (cc in unique(v$col)) {
    xs <- v$dec_x[v$col == cc]
    expect_equal(xs, rep(xs[[1L]], length(xs)))
  }
  # And the offset from the cell centre is one constant for the whole unit --
  # which is what makes it a matrix-wide alignment and not a per-column one.
  expect_equal(v$dec_x - v$x, rep((v$dec_x - v$x)[[1L]], nrow(v)))
})

test_that("every decimal point in a formatting unit lands on the same x -- SANS", {
  # THE test. Space-padding to a common nchar and trusting `family = "mono"`
  # passes the mono case above and silently degrades to merely-centred here,
  # with no error and no warning.
  local_null_pdf()
  graphics::plot.new()
  measure <- measure_base("sans")
  f <- fx(matrix(c(1, 1 / 3, 1000, 0.001, -22.5, 7), nrow = 2))
  r <- resolve(f, panel_fake(7, 7), measure = measure)
  v <- decimal_x(r, measure)

  for (cc in unique(v$col)) {
    xs <- v$dec_x[v$col == cc]
    expect_equal(xs, rep(xs[[1L]], length(xs)))
  }
  expect_equal(v$dec_x - v$x, rep((v$dec_x - v$x)[[1L]], nrow(v)))

  # Non-vacuous: in sans these tokens are NOT all the same width, so a
  # pad-to-nchar scheme genuinely would have failed.
  tok <- paste0(v$sig, v$insig)
  expect_gt(length(unique(round(measure$w(tok, 12), 6))), 1L)
})

test_that("a data frame aligns each column on its OWN decimal point", {
  f <- fx(data.frame(a = c(1, 1 / 3), b = c(1000, 0.001)))
  r <- resolve(f, panel_fake(7, 7))
  v <- decimal_x(r, measure_mono())
  expect_equal(sort(unique(v$fmt_group)), 1:2)
  # One anchor per formatting unit, and the two units differ.
  off <- tapply(v$dec_x - v$x, v$fmt_group, function(z) length(unique(round(z, 12))))
  expect_equal(as.vector(off), c(1L, 1L))
})

# ---------------------------------------------------------------------------
# the cell index sits BELOW the value it names
# ---------------------------------------------------------------------------
#
# A "cellindex" shares its `(row, col)` with the value it names -- same cell, same
# centre -- so `dy_rel` is the ONLY thing keeping the two apart. Drop it and both
# are centred in the same cell and the renderer stamps `[1` + `10` + `1]` on top of
# each other: a grey smear where a number should be. That shipped, because nothing
# asserted the geometry.
#
# So assert it here, in the layout tier, as plain numbers: the resolved `y` of an
# index is strictly below the resolved `y` of its value. No device, no image, no
# eyeballing.

test_that("a cell index resolves BELOW the value it names", {
  panel <- panel_fake(7, 5)
  m <- matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
  v <- c(-3, 5, NA, Inf, 2, 1)

  cases <- list(
    list(m, show_indices = "cell"),
    list(m, show_indices = "all"),
    list(v, show_indices = "inside"),
    list(v, show_indices = "inside", layout = "horizontal")
  )

  for (case in cases) {
    r <- resolve(do.call(fx, case), panel)
    cs <- r$cells

    val <- cs[cs$kind == "value", , drop = FALSE]
    idx <- cs[cs$kind == "cellindex", , drop = FALSE]
    expect_gt(nrow(idx), 0L)
    expect_equal(nrow(idx), nrow(val))

    # Pair each index with the value it names, by the data index -- not by row
    # order, which would still pass if the two chunks were built out of step.
    k <- match(paste(idx$i, idx$j), paste(val$i, val$j))
    expect_false(anyNA(k))
    val <- val[k, , drop = FALSE]

    # BELOW. `y` grows upwards, so the index's `y` is strictly the smaller.
    expect_true(all(idx$y < val$y))
    # And below by exactly one nudge, which is a fraction of a ROW -- so it scales
    # with the cell, on every device.
    expect_equal(idx$y - val$y, rep(r$u * cellindex_dy, nrow(idx)))
    # Vertical only. `x` is the CELL's centre, and the two share a cell -- it is
    # NOT where either span's ink lands, and the two inks are deliberately not
    # centred on each other: the value is anchored on its decimal point through
    # `dx_sig`, the index is centred in the box.
    expect_equal(idx$x, val$x)
    # What IS true of the index's ink: `align = "center"` centres it on the cell.
    w_idx <- measure_mono()$w(idx$sig, idx$fontsize[[1L]])
    expect_equal(idx$dx_sig, -w_idx / 2)
    expect_equal(idx$x + idx$dx_sig + w_idx / 2, idx$x)
    # Still inside the cell it belongs to.
    expect_true(all(idx$y > idx$yb & idx$y < idx$yt))

    # The nudge moved the index and NOTHING ELSE. Every other kind -- value,
    # rowlabel, collabel, ellipsis, outline -- is still dead centre in its box.
    rest <- cs[cs$kind != "cellindex", , drop = FALSE]
    expect_equal(rest$y, (rest$yb + rest$yt) / 2)
    # And the nudge does not move the BOX, only the ink in it.
    expect_equal(idx$yt - idx$yb, rep(r$u, nrow(idx)))
  }
})

# ---------------------------------------------------------------------------
# ...AND IT IS NOT STAMPED THROUGH BY IT
# ---------------------------------------------------------------------------
#
# The test above is an ANCHOR test, and an anchor test cannot see this bug: the
# index's anchor stays 0.2 rows below the value's anchor, and inside its own box,
# while the value is drawn so large that its ink swallows the index whole. Every
# assertion above passes on a picture in which `14` is printed straight through
# `[2, 3]`.
#
# The INK is what has to be asserted. A value and an index that share a cell are a
# STACKED PAIR and are fitted as one unit (`stacked_fontsize()`), so:
#
#     value's bottom edge  >  index's top edge
#
# with real measured text heights, at every size -- not just at the 3x3 that
# happens to clear.

test_that("the value's ink never touches the index inside its cell", {
  m <- measure_mono()
  for (panel in list(panel_fake(7, 7), panel_fake(7, 5), panel_fake(4, 3))) {
    for (case in inside_cases) {
      r <- resolve(do.call(fx, case), panel)
      gap <- ink_gap(r, m)
      expect_true(
        all(gap > 0),
        info = paste0(
          case_label(case), " on ", panel$w_in, "x", panel$h_in,
          "in: worst ink gap ", signif(min(gap), 3), "in"
        )
      )
    }
  }
})

test_that("the stacked pair is what binds the fit, and it binds by SHRINKING the value", {
  # Non-vacuity, and the whole shape of the fix in one assertion. At 7x7in a 6x6
  # with no index inside its cells fits at the `max_pt` cap; ask for the index and
  # the value MUST come down to make room for it. A fit that let the value keep the
  # whole row -- the bug -- would return the same size for both.
  panel <- panel_fake(7, 7)
  bare <- resolve(fx(matrix(1:36, nrow = 6)), panel)
  with_idx <- resolve(fx(matrix(1:36, nrow = 6), show_indices = "cell"), panel)

  expect_equal(bare$fontsize, paint_opts()$max_pt)
  expect_lt(with_idx$fontsize, bare$fontsize)
  # Genuinely fitted, not merely capped.
  expect_lt(with_idx$fontsize, paint_opts()$max_pt)

  # And the size it chose is exactly the stacked-pair budget: the two half-heights
  # meet in the middle of the nudge, less the padding headroom.
  m <- measure_mono()
  h1 <- m$h(height_ref, paint_opts()$ref_pt)[[1L]] / paint_opts()$ref_pt
  v <- with_idx$cells[with_idx$cells$kind == "value", ][1L, ]
  i <- with_idx$cells[with_idx$cells$kind == "cellindex", ][1L, ]
  expect_equal(
    with_idx$fontsize,
    2 * abs(cellindex_dy) * with_idx$u * (1 - paint_opts()$pad) /
      (h1 * (v$size_rel + i$size_rel))
  )

  # A 3x3 is one of the sizes that always cleared, and it is in the documentation.
  # It keeps the biggest text of the lot: the shrink is a function of how tight the
  # cell is, not a tax on every plot that asks for an index.
  bigger <- resolve(fx(matrix(1:9, nrow = 3), show_indices = "cell"), panel)$fontsize
  expect_gt(bigger, with_idx$fontsize)
})

test_that("the honest ink height is paid for by the deeper nudge, not by the reader", {
  # `stacked_fontsize()` fits the pair on the INK (one em), not on `strheight()`'s
  # ascent (0.56 em) -- see `test-ink.R`, which measures both in pixels. That is a
  # ~44% taller height to fit, and on its own it would cost the text a sixth of its
  # size on every plot that draws an index inside a cell.
  #
  # `cellindex_dy` is what pays for it. The bound is LINEAR in the gap, so moving
  # the index from 0.2 rows below the value to 0.3 multiplies the budget by 1.5 --
  # slightly more than the ink model took away. THE TWO CHANGES ARE ONE CHANGE, and
  # this is the assertion that stops one of them being reverted without the other:
  # a collision test cannot see it, because putting the nudge back to 0.2 does not
  # collide, it just quietly shrinks every index plot in the package.
  panel <- panel_fake(7, 5)
  m <- measure_mono()
  opts <- paint_opts()

  # The documented example -- `paint_matrix(mat_3x3, show_indices = "cell")` -- must
  # still fit at the cap. At `cellindex_dy = -0.2` it comes back at about 19.6pt.
  mat_3x3 <- matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
  r3 <- resolve(fx(mat_3x3, show_indices = "cell"), panel, m, opts)
  expect_equal(r3$fontsize, opts$max_pt)

  # And the stacked bound really is the 0.3 one, written out. `measure_mono()`'s
  # `h1` IS exactly `1/72`, so `max(h1, 1/72)` is a no-op here -- which is precisely
  # why `paint_size()`, which fits with `measure_mono()`, is untouched by the ink
  # fix. The nudge, though, it feels.
  r6 <- resolve(fx(matrix(1:36, nrow = 6), show_indices = "cell"), panel, m, opts)
  h1 <- m$h(height_ref, opts$ref_pt)[[1L]] / opts$ref_pt
  expect_equal(h1, 1 / 72)

  v <- r6$cells[r6$cells$kind == "value", ][1L, ]
  i <- r6$cells[r6$cells$kind == "cellindex", ][1L, ]
  expect_equal(
    r6$fontsize,
    2 * 0.3 * r6$u * (1 - opts$pad) / (h1 * (v$size_rel + i$size_rel))
  )
  # Non-vacuous: the 0.2 budget is a DIFFERENT number, and a smaller one.
  expect_gt(
    r6$fontsize,
    2 * 0.2 * r6$u * (1 - opts$pad) / (h1 * (v$size_rel + i$size_rel))
  )
})

test_that("with no index lane, every cell is centred", {
  # The other half of the claim: `dy_rel` is 0 unless there is an index to nudge.
  r <- resolve(fx(matrix(1:6, nrow = 2)), panel_fake(7, 5))
  expect_true(all(r$cells$dy_rel == 0))
  expect_equal(r$cells$y, (r$cells$yb + r$cells$yt) / 2)
})

test_that("with no index INSIDE a cell, the value still gets the WHOLE padded row", {
  # THE COMMON CASE MUST NOT MOVE. `show_indices = "none"/"row"/"column"/"outside"`
  # nudges nothing, so no two spans share a box, so neither the nudge budget nor
  # the stacked pair may reach them: the height available to a cell is
  # `u * (1 - pad)`, exactly as it was.
  #
  # The oracle is that pre-existing fit, written out in full. Shrinking the fonts
  # of every plot in the package would be a steep price for an index nobody asked
  # for, so this is asserted against the old arithmetic itself rather than against
  # a number that a later edit could quietly re-baseline.
  opts <- paint_opts(max_pt = 100)
  m <- measure_mono()
  h1 <- m$h(height_ref, opts$ref_pt)[[1L]] / opts$ref_pt

  # The oracle: `fit_fontsize()` as it was BEFORE the stacked pair existed. `by_h`
  # divides by the raw `h1` -- the ascent -- and NOT by `max(h1, 1/72)`. That is not
  # an oversight, it is the point of the test. The ink floor belongs to
  # `stacked_fontsize()` and to nothing else; put it in `by_h` as well and a plain
  # `paint_vector(letters)` -- no index, no nudge, nothing to protect -- falls from
  # 24pt to 13.6pt on a 7x5in device, because a single-character cell is one of the
  # few places `by_h` genuinely binds.
  unnudged_fit <- function(cells, col_w, u, measure, h_1) {
    req <- cell_demand(cells, measure, opts$ref_pt)
    sz <- cells$size_rel
    by_w <- ifelse(req > 0, u * col_w[cells$col] * (1 - opts$pad) / (req * sz), Inf)
    by_h <- u * (1 - opts$pad) / (h_1 * sz)
    fitting <- which(cells$fit & (nzchar(cells$sig) | nzchar(cells$insig)))
    min(min(pmin(by_w, by_h)[fitting]), opts$max_pt)
  }

  for (case in list(
    list(matrix(1:9, ncol = 1)),
    list(matrix(1:9, ncol = 1), show_indices = "none"),
    list(matrix(1:9, ncol = 1), show_indices = "row"),
    list(matrix(1:9, ncol = 1), show_indices = "column"),
    list(matrix(1:600, nrow = 30, ncol = 20), show_all = TRUE),
    list(iris),
    list(9:1, show_indices = "outside"),
    list(9:1, show_indices = "outside", layout = "horizontal")
  )) {
    f <- do.call(fx, case)
    for (panel in list(panel_fake(7, 5), panel_fake(3, 3), panel_fake(14, 9))) {
      r <- resolve(f, panel, opts = opts)
      expect_true(all(r$cells$dy_rel == 0), info = case_label(case))
      expect_equal(r$fontsize, unnudged_fit(f$cells, f$col_w, r$u, m, h1))

      # AND THE STACKED PAIR CANNOT REACH THESE PLOTS AT ALL. This is the
      # structural half of the lock, and it is what makes the ink fix provably
      # confined: `stacked_fontsize()` is the ONLY thing the fix changed, and it
      # returns `Inf` -- contributes nothing to `min()` -- unless two FITTING cells
      # share a `(row, col)`. The only such pair in the package is value+cellindex.
      # The outline and the ellipsis are `fit = FALSE`; a rowlabel is in column 1;
      # a collabel, a header and a type row are rows of their own. So no edit inside
      # `stacked_fontsize()` can move a pixel of any of these, whatever it does.
      fitting <- which(f$cells$fit & (nzchar(f$cells$sig) | nzchar(f$cells$insig)))
      expect_identical(
        stacked_fontsize(f$cells, fitting, r$u, h1, opts),
        Inf,
        info = case_label(case)
      )
    }
  }

  # ... and it is finite exactly when an index shares a cell, which is what makes
  # the assertion above a claim rather than a tautology.
  for (case in list(
    list(matrix(1:9, ncol = 1), show_indices = "cell"),
    list(matrix(1:9, ncol = 1), show_indices = "all"),
    list(9:1, show_indices = "inside")
  )) {
    f <- do.call(fx, case)
    r <- resolve(f, panel_fake(7, 5), opts = opts)
    fitting <- which(f$cells$fit & (nzchar(f$cells$sig) | nzchar(f$cells$insig)))
    expect_true(
      is.finite(stacked_fontsize(f$cells, fitting, r$u, h1, opts)),
      info = case_label(case)
    )
  }

  # And where the height genuinely binds -- a 9x1 of single digits, whose column
  # sits at the `min_w` floor -- the value's ink fills the padded row exactly. Take
  # so much as the nudge out of that budget and this is what moves.
  r <- resolve(fx(matrix(1:9, ncol = 1)), panel_fake(7, 5), opts = opts)
  expect_equal(r$fontsize, r$u * (1 - opts$pad) / h1)
  v <- r$cells[r$cells$kind == "value", ]
  expect_equal(unique(m$h(v$sig, v$fontsize[[1L]])), r$u * (1 - opts$pad))
})

test_that("the ink floor stays OUT of by_h -- the lock, on a REAL device measure", {
  # THE ORACLE ABOVE HAS A BLIND SPOT, AND IT IS WORTH SAYING OUT LOUD.
  # `measure_mono()`'s `h1` is EXACTLY `1/72`, so `max(h1, 1/72)` is a no-op in the
  # fake font -- which is exactly why `paint_size()` is bit-identical across this
  # change, and exactly why a fake-font oracle CANNOT see an ink floor leaking into
  # `by_h`. A real device can: `strheight("Mg")` is 0.56 em on `pdf()`'s mono, well
  # under the em, so the floor would bite there and nowhere else.
  #
  # It must not bite. `by_h` is the height budget of EVERY cell in the package,
  # index or no index, and a single-character cell is one of the few places it
  # genuinely binds -- so an ink floor in `by_h` takes a plain
  # `paint_vector(letters)`, which has no index, no nudge and nothing whatever to
  # protect, from 24pt down to 13.6pt. The ink floor belongs to the stacked pair.
  local_null_pdf()

  opts <- paint_opts(max_pt = 100)
  m <- measure_base("mono")
  h1 <- m$h(height_ref, opts$ref_pt)[[1L]] / opts$ref_pt

  # Non-vacuous: the real ascent really is under an em, so the floor really would
  # change this arithmetic if it were let in.
  expect_lt(h1, 1 / 72)

  unnudged_fit <- function(cells, col_w, u) {
    req <- cell_demand(cells, m, opts$ref_pt)
    sz <- cells$size_rel
    by_w <- ifelse(req > 0, u * col_w[cells$col] * (1 - opts$pad) / (req * sz), Inf)
    by_h <- u * (1 - opts$pad) / (h1 * sz)
    fitting <- which(cells$fit & (nzchar(cells$sig) | nzchar(cells$insig)))
    min(min(pmin(by_w, by_h)[fitting]), opts$max_pt)
  }

  for (case in list(
    list(letters),                                  # the one that actually moves
    list(matrix(1:9, ncol = 1)),
    list(matrix(1:9, ncol = 1), show_indices = "row"),
    list(matrix(1:9, ncol = 1), show_indices = "column"),
    list(matrix(1:600, nrow = 30, ncol = 20), show_all = TRUE),
    list(iris),
    list(9:1, show_indices = "outside")
  )) {
    f <- do.call(fx, case)
    for (panel in list(panel_fake(6.8, 4.3), panel_fake(3, 3), panel_fake(14, 9))) {
      r <- resolve(f, panel, m, opts)
      expect_true(all(r$cells$dy_rel == 0), info = case_label(case))
      expect_equal(
        r$fontsize,
        unnudged_fit(f$cells, f$col_w, r$u),
        info = case_label(case)
      )
    }
  }
})

# ---------------------------------------------------------------------------
# cross-backend agreement -- base and ggplot draw the same picture
# ---------------------------------------------------------------------------

test_that("paint_resolve() agrees between measure_base() and measure_grid()", {
  local_null_pdf()
  graphics::plot.new()
  panel <- panel_fake(7, 5)

  for (family in c("mono", "sans")) {
    for (data in list(
      matrix(c(123456.789, 1 / 3, 100000, 0.5), nrow = 2),
      data.frame(n = c(1.5, 20), s = c("ab", "cdef"), l = c(TRUE, NA),
                 stringsAsFactors = FALSE),
      letters[1:6]
    )) {
      f <- fx(data)
      rb <- resolve(f, panel, measure = measure_base(family),
                    opts = paint_opts(family = family))
      rg <- resolve(f, panel, measure = measure_grid(family),
                    opts = paint_opts(family = family))
      expect_equal(rb$fontsize, rg$fontsize, tolerance = 1e-10)
      expect_equal(rb$floored, rg$floored)
      expect_equal(rb$u, rg$u)
      expect_equal(rb$x0, rg$x0)
      expect_equal(rb$y0, rg$y0)
      expect_equal(rb$cells, rg$cells, tolerance = 1e-10)
    }
  }
})

test_that("base and grid agree, and neither collides, with the index INSIDE the cell", {
  # THE DRIFT GUARD, at sizes that BREAK. The stacked-pair fit lives in
  # `paint_resolve()` -- shared -- and the two backends differ only in their
  # measure, so a fix applied to one and not the other is impossible by
  # construction. This is what proves it stayed that way, and it is fixtured at
  # 6x6 and 20x20 and a 20-vector rather than at the 3x3 that clears anyway.
  #
  # Real measures, on a real device: `measure_mono()` is an exactly linear fake
  # font, and the onset of the collision is a question about real font metrics.
  local_null_pdf()
  graphics::plot.new()
  panel <- panel_fake(7, 7)

  for (case in inside_cases) {
    f <- do.call(fx, case)
    for (family in c("mono", "sans")) {
      mb <- measure_base(family)
      mg <- measure_grid(family)
      opts <- paint_opts(family = family)
      rb <- resolve(f, panel, measure = mb, opts = opts)
      rg <- resolve(f, panel, measure = mg, opts = opts)

      expect_equal(rb$fontsize, rg$fontsize, tolerance = 1e-10)
      expect_equal(rb$cells, rg$cells, tolerance = 1e-10)

      lab <- paste(case_label(case), family)
      expect_true(all(ink_gap(rb, mb) > 0), info = paste("base", lab))
      expect_true(all(ink_gap(rg, mg) > 0), info = paste("grid", lab))
    }
  }

  # With REAL metrics, the 3x3 of the documentation -- `paint_matrix(mat_3x3,
  # show_indices = "cell")` -- is still drawn at the `max_pt` cap. Its cells were
  # always roomy enough for both spans, and the pair fit takes nothing from a pair
  # that already fits.
  r3 <- resolve(
    fx(matrix(1:9, nrow = 3), show_indices = "cell"), panel,
    measure = measure_base("mono"), opts = paint_opts(family = "mono")
  )
  expect_equal(r3$fontsize, paint_opts()$max_pt)
})

# ---------------------------------------------------------------------------
# the resolved table
# ---------------------------------------------------------------------------

test_that("paint_resolve() adds its fields and keeps the table bare", {
  f <- fx(matrix(1:6, nrow = 2))
  r <- resolve(f, panel_fake(7, 5))

  expect_named(r, c("cells", "fontsize", "floored", "u", "x0", "y0"))
  expect_s3_class(r$cells, "data.frame", exact = TRUE)
  expect_true(all(
    c("fontsize", "dx_sig", "dx_insig", "x", "y", "xl", "xr", "yb", "yt") %in%
      names(r$cells)
  ))
  expect_equal(nrow(r$cells), nrow(f$cells))
  expect_false(any(is.na(r$cells$dx_sig)))
  expect_false(any(is.na(r$cells$dx_insig)))
  expect_false(any(is.na(r$cells$fontsize)))

  # `fit = FALSE` excludes a cell from the fitting decision, but it still gets a
  # size -- the "..." has to be drawn at something.
  el <- fx(matrix(1:900, nrow = 30, ncol = 30))
  re <- resolve(el, panel_fake(7, 7))
  gaps <- re$cells[re$cells$kind == "ellipsis", ]
  expect_gt(nrow(gaps), 0L)
  expect_true(all(gaps$fontsize > 0))
  expect_false(any(gaps$fit))
})

test_that("the outline rectangle spans the whole value block", {
  f <- fx(matrix(1:6, nrow = 2, ncol = 3), show_indices = "all")
  r <- resolve(f, panel_fake(7, 7))
  o <- r$cells[r$cells$kind == "outline", ]
  b <- r$cells[r$cells$kind %in% c("value", "ellipsis"), ]
  expect_equal(nrow(o), 1L)
  expect_equal(o$xl, min(b$xl))
  expect_equal(o$xr, max(b$xr))
  expect_equal(o$yb, min(b$yb))
  expect_equal(o$yt, max(b$yt))
  # It does not swallow the row/column label gutters.
  lab <- r$cells[r$cells$kind %in% c("rowlabel", "collabel"), ]
  expect_gt(nrow(lab), 0L)
  expect_true(any(lab$xl < o$xl) || any(lab$yt > o$yt))
})

test_that("row 1 is the TOP row", {
  f <- fx(matrix(1:6, nrow = 3, ncol = 2))
  r <- resolve(f, panel_fake(7, 7))
  v <- r$cells[r$cells$kind == "value", ]
  y_by_row <- tapply(v$y, v$row, function(z) z[[1L]])
  expect_true(all(diff(as.vector(y_by_row)) < 0))
})

test_that("paint_resolve() rejects a col_w that is too short", {
  f <- fx(matrix(1:6, nrow = 2))
  expect_error(
    paint_resolve(f$cells, f$col_w[1:2], f$n_row, panel_fake(7, 7), measure_mono()),
    "widths"
  )
})

test_that("paint_opts() validates", {
  expect_error(paint_opts(fontsize = 0), "positive")
  expect_error(paint_opts(fontsize = c(1, 2)), "positive")
  expect_error(paint_opts(min_pt = -1), "positive")
  expect_error(paint_opts(max_pt = 1, min_pt = 5), "at least")
  expect_error(paint_opts(pad = 1), "between 0 and 1")
  expect_error(paint_opts(ref_pt = 0), "positive")
  expect_null(paint_opts()$fontsize)
  expect_equal(paint_opts()$min_pt, 5)
})

# ---------------------------------------------------------------------------
# the whole pipeline, on a real device, character data included (bug 1)
# ---------------------------------------------------------------------------

test_that("every structure resolves on a live device without NA or error", {
  local_null_pdf()
  graphics::plot.new()
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
    for (measure in list(measure_base("mono"), measure_grid("sans"), measure_mono())) {
      r <- paint_resolve(f$cells, f$col_w, f$n_row, panel_fake(7, 5), measure)
      expect_false(any(is.na(r$cells$dx_sig)))
      expect_false(any(is.na(r$cells$dx_insig)))
      expect_true(all(is.finite(r$cells$fontsize)))
      expect_true(is.finite(r$fontsize) && r$fontsize > 0)
    }
  }
})
