# Tier 4: THE INK. The one thing that cannot be proved by arithmetic, because the
# arithmetic is what was wrong.
#
# `test-layout.R` already asserts that the value's ink clears the index's, and it
# passed all along -- on a picture in which the value was stamped straight through
# the index. It could not do otherwise: it measures the ink with the SAME closure
# the fit uses, so a fit that under-models the ink is checked against a model that
# under-models it identically. The check was the bug, wearing a hat.
#
# `graphics::strheight()` and `grid::grobHeight()` report the FONT ASCENT. Both
# renderers anchor text with `adj = c(0, 0.5)` / `vjust = 0.5`, which centres a
# string's TRUE INK on the anchor. The two are different numbers, and `[i, j]` --
# brackets and a comma, reaching above the digits and below the baseline -- is the
# worst string in the package for it. Rasterised and counted, in `"mono"`:
#
#     "Mg"      0.79 em ink   vs   0.56 em ascent
#     "-123.4"  0.66 em ink   vs   0.56 em ascent
#     "[1, 1]"  0.82 em ink   vs   0.56 em ascent      <- 46% taller
#
# So this file does not ask a font metric anything. It DRAWS the string, reads the
# pixels back, and finds the ink. Measured that way, before the fix, a 6x6 numeric
# matrix at `show_indices = "all"` on 7x5in put the value's ink 0.0029in BELOW the
# top of its index's ink -- a strike-through, sub-pixel on screen and unmistakable
# in print. It hits numerics HARDER than characters: a decimal-aligned block is
# centred and the index is centred, so they overlap in x maximally.
#
# `bmp()` is the device because it writes an UNCOMPRESSED raster, so `readBin()` is
# the whole decoder -- no `png` package, and no `dev.capture()`, which the macOS
# png device does not implement.

# Which rows of a BMP carry ink? Rows are counted from the TOP.
bmp_ink_rows <- function(file) {
  r <- readBin(file, "raw", file.size(file))
  off <- readBin(r[11:14], "integer", 1L, size = 4L, endian = "little")
  w <- readBin(r[19:22], "integer", 1L, size = 4L, endian = "little")
  h <- readBin(r[23:26], "integer", 1L, size = 4L, endian = "little")
  bpp <- readBin(r[29:30], "integer", 1L, size = 2L, signed = FALSE, endian = "little")
  if (!bpp %in% c(24L, 32L)) {
    return(NULL)
  }
  # A negative height means the rows are stored top-down.
  top_down <- h < 0L
  h <- abs(h)
  stride <- floor((bpp * w + 31L) / 32L) * 4L

  body <- r[(off + 1L):(off + stride * h)]
  m <- matrix(body, nrow = stride)
  # The colour bytes of each pixel are the first three of its group; the rest of
  # the stride is alpha and padding, and white is 0xff in every channel.
  per <- bpp %/% 8L
  cols <- rep(seq_len(w) - 1L, each = 3L) * per + rep(1:3, times = w)
  ink <- colSums(m[cols, , drop = FALSE] != as.raw(255L)) > 0

  rows <- which(ink)
  if (!top_down) {
    rows <- h - rows + 1L
  }
  list(rows = rows, h = h)
}

# The TRUE ink span of a string, in inches, RELATIVE TO ITS ANCHOR, drawn by the
# backend's own text primitive with the backend's own anchoring. Positive is above.
ink_span <- function(s, pt, backend, family = "mono", res = 600,
                     w_in = 12, h_in = 1.2) {
  f <- tempfile(fileext = ".bmp")
  on.exit(unlink(f), add = TRUE)

  grDevices::bmp(f, width = w_in, height = h_in, units = "in", res = res, bg = "white")
  ok <- FALSE
  on.exit(if (!ok) try(grDevices::dev.off(), silent = TRUE), add = TRUE)

  anchor <- h_in / 2
  if (identical(backend, "base")) {
    graphics::par(mai = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::plot.window(
      xlim = c(0, w_in), ylim = c(0, h_in),
      xaxs = "i", yaxs = "i"
    )
    # EXACTLY `draw_base()`'s anchoring.
    graphics::text(
      x = 0.2, y = anchor, labels = s, adj = c(0, 0.5),
      cex = pt / graphics::par("ps"), family = family
    )
  } else {
    # EXACTLY `paintr_children()`'s anchoring.
    grid::grid.text(
      s,
      x = grid::unit(0.2, "in"), y = grid::unit(anchor, "in"),
      hjust = 0, vjust = 0.5,
      gp = grid::gpar(fontsize = pt, fontfamily = family)
    )
  }
  grDevices::dev.off()
  ok <- TRUE

  z <- bmp_ink_rows(f)
  if (is.null(z) || length(z$rows) == 0L) {
    return(NULL)
  }
  c(
    bottom = (z$h - max(z$rows)) / res - anchor,
    top = (z$h - (min(z$rows) - 1L)) / res - anchor
  )
}

# The envelope of every glyph a set of strings can draw, as one string. A string's
# ink box is the union of its glyphs' ink boxes, and where a glyph sits
# horizontally cannot change how high or low it reaches -- so the span of the
# concatenated alphabet BOUNDS every one of those strings at once, exactly, in one
# raster instead of four hundred.
glyph_envelope <- function(strings) {
  ch <- unique(unlist(strsplit(paste(strings, collapse = ""), "", fixed = TRUE)))
  paste(sort(ch[nzchar(ch)]), collapse = "")
}

can_raster <- function() {
  f <- tempfile(fileext = ".bmp")
  on.exit(unlink(f), add = TRUE)
  ok <- tryCatch({
    grDevices::bmp(f, width = 2, height = 2, units = "in", res = 50)
    # `mai` first: the default margins do not fit on a canvas this small, and
    # `plot.new()` would error with "figure margins too large" on a device that is
    # working perfectly well.
    graphics::par(mai = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Mg")
    grDevices::dev.off()
    TRUE
  }, error = function(e) FALSE)
  isTRUE(ok) && file.exists(f) && !is.null(bmp_ink_rows(f))
}

# ---------------------------------------------------------------------------
# the assertion
# ---------------------------------------------------------------------------

# The clearance between the value's ink and the index's, in inches, on a resolved
# cell table. Positive is clear air; negative is a strike-through.
#
# Every (value, index) pair has the SAME vertical relationship -- the value sits at
# its cell's centre and the index `cellindex_dy` rows below it -- so one number
# describes the whole plot, and taking the glyph envelope of each makes that number
# the WORST CASE over every cell.
ink_clearance <- function(res, backend, family = "mono") {
  cs <- res$cells
  val <- cs[cs$kind == "value", , drop = FALSE]
  idx <- cs[cs$kind == "cellindex", , drop = FALSE]
  stopifnot(nrow(idx) > 0L, nrow(val) > 0L)

  sv <- ink_span(glyph_envelope(paste0(val$sig, val$insig)), val$fontsize[[1L]], backend, family)
  si <- ink_span(glyph_envelope(idx$sig), idx$fontsize[[1L]], backend, family)
  if (is.null(sv) || is.null(si)) {
    return(NULL)
  }

  # Pair by the DATA index, so a pairing that had drifted would not line up.
  k <- match(paste(idx$i, idx$j), paste(val$i, val$j))
  stopifnot(!anyNA(k))
  min((val$y[k] + sv[["bottom"]]) - (idx$y + si[["top"]]))
}

ink_cases <- function() {
  set.seed(20240607)
  states <- c("Massachusetts", "Rhode Island", "Connecticut", "Vermont",
              "New Hampshire", "Maine", "New Jersey", "New York", "Wyoming")
  list(
    list(nm = "numeric 3x3 cell",  d = matrix(rnorm(9) * 100, 3),    si = "cell"),
    list(nm = "numeric 3x3 all",   d = matrix(rnorm(9) * 100, 3),    si = "all"),
    list(nm = "numeric 6x6 all",   d = matrix(rnorm(36) * 100, 6),   si = "all"),
    list(nm = "numeric 20x20 all", d = matrix(rnorm(400) * 100, 20), si = "all"),
    list(nm = "chr 3x3 all",       d = matrix(states, 3),            si = "all"),
    list(nm = "chr 6x6 all",       d = matrix(rep(states, 4), 6),    si = "all"),
    list(nm = "chr 20x20 all",     d = matrix(rep(states, 45)[1:400], 20), si = "all"),
    list(nm = "vector 1:20 inside", d = 1:20,                        si = "inside")
  )
}

test_that("the value's TRUE INK never strikes through its index -- BOTH backends", {
  skip_on_cran()
  skip_if_not(can_raster(), "no readable bmp device")

  opts <- paint_opts(family = "mono")

  for (backend in c("base", "grid")) {
    measure_of <- if (backend == "base") measure_base else measure_grid

    for (cs in ink_cases()) {
      # The fit and the ink must be measured with the SAME font metrics, so the
      # resolve happens on a `bmp()` device too -- `pdf()`'s Courier is not the
      # system mono the raster will actually draw.
      f <- tempfile(fileext = ".bmp")
      grDevices::bmp(f, width = 7, height = 5, units = "in", res = 100)
      dev <- grDevices::dev.cur()

      cells <- paint_cells(cs$d, show_indices = cs$si)
      r <- paint_resolve(
        cells, column_widths(cells), attr(cells, "n_row"),
        panel = panel_fake(6.6, 4.4),
        measure = measure_of("mono"),
        opts = opts
      )

      grDevices::dev.off(dev)
      unlink(f)

      gap <- ink_clearance(r, backend)
      skip_if(is.null(gap), "no ink captured")

      info <- paste0(
        backend, ": ", cs$nm, " -- fitted ", signif(r$fontsize, 4),
        "pt, ink clearance ", signif(gap, 3), "in (", signif(gap / r$u, 3),
        " of a row)"
      )

      # THE CLAIM: the value's ink stops above the index's ink.
      expect_gt(gap, 0)

      # AND IT IS A REAL GAP, not a sub-pixel accident. This is the assertion that
      # bites hardest. Reverting the fix leaves four of these eight cases frankly
      # overlapping and the other four "clear" by 0.002 to 0.004 of a row height --
      # a fraction of one pixel, which antialiasing smears into a strike-through
      # regardless. A clearance is only a clearance if you can see it.
      #
      # 0.04 of a row is the line, and it discriminates: every case here clears it
      # (0.049 to 0.129) and every case fails it on the old arithmetic. The
      # CHARACTER cases are the tight ones -- the glyph envelope of "New Jersey" and
      # "Wyoming" carries a `y` and a `J`, whose descenders reach further below the
      # anchor than any digit does -- which is exactly why they are in the fixture.
      expect_gt(gap / r$u, 0.04)
      expect_true(TRUE, info = info)
    }
  }
})

test_that("base and grid put the same ink in the same place", {
  # The cross-backend claim, made on PIXELS rather than on the two measure
  # closures agreeing with each other. If these two ever diverge, the package is
  # drawing two different pictures from one cell table.
  skip_on_cran()
  skip_if_not(can_raster(), "no readable bmp device")

  for (s in c("Mg", "[1, 1]", "-123.4", "[10, 10]")) {
    b <- ink_span(s, 16, "base")
    g <- ink_span(s, 16, "grid")
    skip_if(is.null(b) || is.null(g), "no ink captured")
    # One raster row of tolerance, at 600dpi.
    expect_equal(b[["top"]], g[["top"]], tolerance = 1e-3, info = s)
    expect_equal(b[["bottom"]], g[["bottom"]], tolerance = 1e-3, info = s)
  }
})

test_that("the ink of `[i, j]` really is taller than the ascent the fit was using", {
  # The ROOT CAUSE, asserted as a number, so that a future edit that quietly goes
  # back to fitting the pair on `strheight()` has to argue with this.
  skip_on_cran()
  skip_if_not(can_raster(), "no readable bmp device")

  pt <- 24
  em <- pt / 72

  ink <- ink_span("[1, 1]", pt, "base")
  skip_if(is.null(ink), "no ink captured")
  ink_h <- ink[["top"]] - ink[["bottom"]]

  grDevices::bmp(tempfile(fileext = ".bmp"), width = 4, height = 4,
                 units = "in", res = 72)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  ascent <- measure_base("mono")$h(height_ref, pt)[[1L]]

  # The ascent is what `fit_fontsize()` is handed. The ink is what gets drawn.
  expect_gt(ink_h, ascent)
  expect_gt(ink_h / ascent, 1.3)
  # And one em bounds it -- which is exactly why `stacked_fontsize()` floors the
  # height at `1 / 72` per point, and why that floor is not merely a fudge.
  expect_lt(ink_h, em)
  expect_lt(ascent, em)
})
