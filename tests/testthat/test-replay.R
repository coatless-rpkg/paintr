# Tier 4: REPLAY. The base backend must draw the same picture when it is RECORDED
# on one device and REPLAYED onto a device of a different size AND aspect.
#
# This is not an exotic case: it is the package's primary medium. `knitr`,
# `rmarkdown`, `quarto`, `pkgdown` and the R help viewer all run your code on a
# recording device, `recordPlot()` it, and `replayPlot()` it onto the real figure
# device -- and those two devices are almost never the same size. `pkgdown` renders
# an `@examples` block through `evaluate::evaluate()`, whose recording device
# defaults to 6.667 x 6.667in, then replays it onto its figure device, which
# defaults to 7.292 x 4.507in. Different size, different aspect, every time.
#
# The bug this file pins: `render_base()` used to fit the font and place the text
# on the RECORDING device and bake both into the display list. A baked point size
# does not rescale, and a baked inch offset is divided by the recording device's
# inch->user ratio -- so on replay the rectangles re-letterboxed correctly (their
# coordinates are pure layout units) but the text kept the recording device's size
# and anchor, and overran its cells. Measured on the reported figure: the `FALSE`
# glyph ended 48px PAST the right edge of its own cell.
#
# The fix defers the whole fit-and-draw into `grDevices::recordGraphics()`, which
# re-evaluates it on every replay -- the same deferral `makeContent.paintr_grob()`
# already does for grid. So this file proves, IN PIXELS, that a recorded-then-
# replayed base plot is identical to one drawn directly onto the replay device, for
# every painter, both alignments, and with and without a highlight.
#
# `bmp()` is the device because it writes an UNCOMPRESSED raster, so `readBin()` is
# the whole decoder -- no `png` package, no `dev.capture()`. The same choice
# `test-ink.R` makes.

# Read a BMP as an [h, w, 3] integer array of R, G, B. Rows count from the TOP.
# Handles the 24bpp packed form `grDevices::bmp()` writes.
read_bmp_rgb <- function(file) {
  r <- readBin(file, "raw", file.size(file))
  off <- readBin(r[11:14], "integer", 1L, size = 4L, endian = "little")
  w <- readBin(r[19:22], "integer", 1L, size = 4L, endian = "little")
  h <- readBin(r[23:26], "integer", 1L, size = 4L, endian = "little")
  bpp <- readBin(r[29:30], "integer", 1L, size = 2L, signed = FALSE, endian = "little")
  if (!bpp %in% c(24L, 32L)) {
    return(NULL)
  }
  top_down <- h < 0L
  h <- abs(h)
  per <- bpp %/% 8L
  stride <- floor((bpp * w + 31L) / 32L) * 4L
  body <- as.integer(r[(off + 1L):(off + stride * h)])
  m <- matrix(body, nrow = stride)
  arr <- array(0L, c(h, w, 3L))
  # BMP stores each pixel as B, G, R (, A). Map to R, G, B.
  for (ch in 1:3) {
    rows <- (seq_len(w) - 1L) * per + (4L - ch)
    img <- t(m[rows, , drop = FALSE])
    if (!top_down) {
      img <- img[h:1, , drop = FALSE]
    }
    arr[, , ch] <- img
  }
  arr
}

# Can we open a readable bmp device on this machine? Mirrors `test-ink.R`.
can_raster_rgb <- function() {
  f <- tempfile(fileext = ".bmp")
  on.exit(unlink(f), add = TRUE)
  ok <- tryCatch({
    grDevices::bmp(f, width = 2, height = 2, units = "in", res = 50, bg = "white")
    graphics::par(mai = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::rect(0.2, 0.2, 0.8, 0.8, col = "red", border = NA)
    grDevices::dev.off()
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok) || !file.exists(f)) {
    return(FALSE)
  }
  a <- read_bmp_rgb(f)
  # The red rectangle must read back as red, or the decoder disagrees with the
  # device and every assertion below is meaningless.
  !is.null(a) && any(a[, , 1] > 200 & a[, , 2] < 80 & a[, , 3] < 80)
}

# Draw `fn` directly onto a `w_in x h_in` bmp and read the pixels back.
raster_direct <- function(fn, w_in, h_in, res = 100) {
  f <- tempfile(fileext = ".bmp")
  on.exit(unlink(f), add = TRUE)
  grDevices::bmp(f, width = w_in, height = h_in, units = "in", res = res, bg = "white")
  fn()
  grDevices::dev.off()
  read_bmp_rgb(f)
}

# RECORD `fn` on a `rec` device, REPLAY it onto a `tgt` device, read the pixels of
# the replay. `rec` and `tgt` are `c(width_in, height_in)`.
raster_replay <- function(fn, rec, tgt, res = 100) {
  f1 <- tempfile(fileext = ".bmp")
  grDevices::bmp(f1, width = rec[[1L]], height = rec[[2L]], units = "in", res = res, bg = "white")
  grDevices::dev.control("enable")
  fn()
  p <- grDevices::recordPlot()
  grDevices::dev.off()
  unlink(f1)

  f2 <- tempfile(fileext = ".bmp")
  on.exit(unlink(f2), add = TRUE)
  grDevices::bmp(f2, width = tgt[[1L]], height = tgt[[2L]], units = "in", res = res, bg = "white")
  grDevices::replayPlot(p)
  grDevices::dev.off()
  read_bmp_rgb(f2)
}

# Fraction of pixels that differ by more than `thr` on any channel.
frac_diff <- function(a, b, thr = 8L) {
  if (is.null(a) || is.null(b) || !all(dim(a) == dim(b))) {
    return(1)
  }
  d <- abs(a - b)
  worst <- pmax(d[, , 1], d[, , 2], d[, , 3])
  sum(worst > thr) / length(worst)
}

# The painters, one per structure, both alignments, with and without a highlight.
# `decimal` numerics and `center` characters are both present, in a matrix and
# again folded into the one mixed data frame.
replay_cases <- function() {
  m_num <- matrix(c(1.5, 22.33, -3, 400, 5.1, 6), nrow = 2)
  m_chr <- matrix(c("aa", "bbb", "c", "dd", "ee", "f"), nrow = 2)
  df <- data.frame(
    x = c(1.5, 22.25, -3),
    y = c("aa", "bb", "ccc"),
    stringsAsFactors = FALSE
  )
  v <- letters[1:6]
  l <- list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA))
  list(
    list(nm = "vector", fn = function() paint_vector(v)),
    list(nm = "vector + highlight",
         fn = function() paint_vector(v, highlight_area = highlight_locations(v, 2:4))),
    list(nm = "matrix numeric, index in cell",
         fn = function() paint_matrix(m_num, show_indices = "cell")),
    list(nm = "matrix numeric, index alongside",
         fn = function() paint_matrix(m_num, show_indices = "all")),
    list(nm = "matrix numeric + highlight",
         fn = function() paint_matrix(m_num, highlight_area = highlight_locations(m_num, cbind(1, 2)))),
    list(nm = "matrix character",
         fn = function() paint_matrix(m_chr, show_indices = "all")),
    list(nm = "data frame (mixed alignments)",
         fn = function() paint_data_frame(df)),
    list(nm = "data frame + highlight",
         fn = function() paint_data_frame(df, highlight_area = highlight_columns(df, "x"))),
    list(nm = "list", fn = function() paint_list(l)),
    list(nm = "list + highlight (the reported case)",
         fn = function() paint_list(l, highlight_area = highlight_columns(l, "c"))),
    list(nm = "array (rank 3)", fn = function() paint_array(Titanic))
  )
}

test_that("a recorded base plot replays IDENTICALLY onto a device of a different size and aspect", {
  skip_on_cran()
  skip_if_not(can_raster_rgb(), "no readable bmp device")

  # Both pairs change the size AND the aspect between record and replay: a square
  # recording device to a wide one, and a tall one to a wide one. This is exactly
  # what knitr/pkgdown do -- and on the buggy code every painter diverged here by
  # 1% to 9% of its pixels (vector 3252px, list 11133px, array 30966px).
  pairs <- list(
    list(rec = c(7, 7),   tgt = c(5.5, 3.4)),
    list(rec = c(4, 6),   tgt = c(8, 4))
  )

  for (cs in replay_cases()) {
    for (pr in pairs) {
      direct <- raster_direct(cs$fn, pr$tgt[[1L]], pr$tgt[[2L]])
      replay <- raster_replay(cs$fn, pr$rec, pr$tgt)
      skip_if(is.null(direct) || is.null(replay), "no ink captured")

      f <- frac_diff(direct, replay)
      info <- paste0(
        cs$nm, ": record ", pr$rec[[1L]], "x", pr$rec[[2L]],
        " -> replay ", pr$tgt[[1L]], "x", pr$tgt[[2L]],
        " differ ", signif(100 * f, 3), "%"
      )
      # The replay is the direct draw. Measured 0% on the fix; the SMALLEST bug in
      # the fixture is ~1% of pixels, so this threshold discriminates with room to
      # spare and still tolerates a stray antialiased edge pixel.
      expect_lt(f, 0.003, label = info)
    }
  }
})

# ---------------------------------------------------------------------------
# the reported symptom, in pixels: text ink outside the cell FILL
# ---------------------------------------------------------------------------

# How far, in pixels, dark text ink protrudes to the RIGHT of the highlighted
# cell's fill. The highlight fill is `lemonchiffon` = (255, 250, 205), and the
# rightmost highlighted column is `c` -- so any dark ink to the right of the fill,
# in the fill's own rows, is a value glyph that has left its cell. This is the
# reported symptom read straight off the raster, not off `strwidth()`.
fill_right_overrun <- function(img) {
  R <- img[, , 1]
  G <- img[, , 2]
  B <- img[, , 3]
  fill <- (R > 235) & (G > 230) & (B > 170) & (B < 225)
  dark <- (R < 120) & (G < 120) & (B < 120)
  fw <- which(fill, arr.ind = TRUE)
  if (nrow(fw) == 0L) {
    return(NA_integer_)
  }
  rt <- min(fw[, 1L])
  rb <- max(fw[, 1L])
  cr <- max(fw[, 2L])
  dw <- which(dark, arr.ind = TRUE)
  band <- dw[dw[, 1L] >= rt & dw[, 1L] <= rb & dw[, 2L] > cr, , drop = FALSE]
  if (nrow(band) == 0L) {
    return(0L)
  }
  max(band[, 2L]) - cr
}

test_that("replayed text ink stays inside the highlighted cell -- the reported bug", {
  skip_on_cran()
  skip_if_not(can_raster_rgb(), "no readable bmp device")

  l <- list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA))
  fn <- function() paint_list(l, highlight_area = highlight_columns(l, "c"))

  # The reported geometry: record on a 7x7 square, replay onto pkgdown's figure.
  tgt <- c(7.291667, 4.506593)
  direct <- raster_direct(fn, tgt[[1L]], tgt[[2L]], res = 192)
  replay <- raster_replay(fn, c(7, 7), tgt, res = 192)
  skip_if(is.null(direct) || is.null(replay), "no ink captured")

  d_over <- fill_right_overrun(direct)
  r_over <- fill_right_overrun(replay)
  skip_if(is.na(d_over) || is.na(r_over), "no fill captured")

  # Non-vacuous: the direct draw is correct (the glyph stops a pixel or two inside
  # the fill), so a passing replay is a real result, not an empty raster.
  expect_lt(d_over, 6L)
  # THE CLAIM. On the bug this was ~48px on replay while the direct draw was ~2px.
  expect_lt(r_over, 6L)
})

test_that("an @examples-style record/replay through evaluate() keeps text in its cells", {
  # THE MEDIUM THAT MATTERS. `pkgdown` renders an `@examples` block by running it
  # through `evaluate::evaluate()`, whose recording device is `ragg::agg_record()`
  # (or `pdf(NULL)` without ragg) -- neither the size nor the aspect of the figure
  # it is finally drawn onto. This reproduces that exact pipeline: evaluate the
  # code, take the `recordedplot` it captured, and replay it onto pkgdown's default
  # figure device. On the buggy code the `FALSE` glyph ended 48px past its cell.
  skip_on_cran()
  skip_if_not_installed("evaluate")
  skip_if_not(can_raster_rgb(), "no readable bmp device")

  code <- paste(
    "l <- list(a = 1:4, b = 'x', c = c(TRUE, FALSE, NA))",
    "paint_list(l, highlight_area = highlight_columns(l, 'c'))",
    sep = "\n"
  )
  outs <- evaluate::evaluate(code, envir = environment())
  rp <- Filter(function(o) inherits(o, "recordedplot"), outs)
  skip_if(length(rp) == 0L, "evaluate() captured no plot")

  f <- tempfile(fileext = ".bmp")
  on.exit(unlink(f), add = TRUE)
  grDevices::bmp(f, width = 7.291667, height = 4.506593, units = "in", res = 192, bg = "white")
  grDevices::replayPlot(rp[[1L]])
  grDevices::dev.off()

  over <- fill_right_overrun(read_bmp_rgb(f))
  skip_if(is.na(over), "no fill captured")
  expect_lt(over, 6L)
})
