# Tier 3: the grob, as text. No images, no vdiffr, and NO GGPLOT2.
#
# This is the highest-value test in the suite, and the reason `render-grid.R` is
# forbidden from mentioning ggplot2. `makeContent()` is an ordinary function; a
# viewport is an ordinary object; so the deferred font fit -- the thing that
# looks like it could only be checked by diffing pictures -- is checkable by
# reading numbers out of a list.
#
# It also pins the two facts that a picture would NOT have caught:
#
#   * autofit is monotone in device size (the same object refits, and refits
#     upward, as the device grows);
#   * the floor warning fires EXACTLY ONCE per grob, no matter how many times the
#     object is drawn.
#
# NEVER snapshot the numeric font size. `pdf()` quantizes font size to integer
# points (a requested 3.6pt renders at 4pt), so the number is not reproducible
# across devices; only its ORDERING is.

# A d x d pdf device with a d x d viewport pushed: the panel is exactly d inches
# square, which is what `panel_grid()` will read back.
with_panel <- function(d, expr) {
  grDevices::pdf(NULL, width = d, height = d)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    width = grid::unit(d, "in"),
    height = grid::unit(d, "in")
  ))
  on.exit(grid::popViewport(), add = TRUE, after = FALSE)
  force(expr)
}

fx <- function(x, ...) {
  cells <- paint_cells(x, ...)
  list(
    cells = cells,
    col_w = column_widths(cells),
    n_row = attr(cells, "n_row")
  )
}

grob_of <- function(x, ..., opts = paint_opts(), warn_floor = TRUE) {
  f <- fx(x, ...)
  paintr_grob(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = warn_floor)
}

# A grid dense enough that autofit never hits the max_pt cap, so the size is a
# strictly increasing function of the device.
dense <- function(...) grob_of(matrix(1:600, nrow = 30, ncol = 20), show_all = TRUE, ...)

kid <- function(kids, nm) {
  hit <- Filter(function(g) identical(g$name, nm), kids)
  if (length(hit) == 0L) NULL else hit[[1L]]
}

# The base font size, read back off the drawn children. Value cells and the
# "..." carry size_rel = 1 and labels carry less, so the maximum IS the base
# size -- and reading it from the CHILDREN rather than from `paint_resolve()`
# is the point: it is what actually reaches the device.
sig_fontsize <- function(kids) max(kid(kids, "paintr.sig")$gp$fontsize)

# What `draw_base()` actually hands `graphics::rect()`.
#
# Base graphics draws into a device and forgets, so there is no drawn object to
# interrogate the way there is in grid -- the only place a base rectangle can be
# read back is at the call itself. Mocking `graphics::rect` is therefore not a
# shortcut around the renderer: it IS the renderer's output, caught at the one
# moment it exists.
base_rects <- function(f, opts = paint_opts()) {
  got <- NULL
  testthat::local_mocked_bindings(
    rect = function(...) {
      got <<- list(...)
      invisible(NULL)
    },
    .package = "graphics"
  )
  grDevices::pdf(NULL, width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  render_base(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
  got
}

# What `draw_base()` actually hands `graphics::text()`, in call order: the black span
# first, the grey span second. Same trick as `base_rects()`, and for the same reason --
# a base label exists only at the moment of the call.
base_text <- function(f, opts = paint_opts()) {
  got <- list()
  testthat::local_mocked_bindings(
    text = function(...) {
      got[[length(got) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "graphics"
  )
  grDevices::pdf(NULL, width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  render_base(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
  got
}

# ---------------------------------------------------------------------------
# construction: pure, and it touches no device
# ---------------------------------------------------------------------------

test_that("paintr_grob() is a gTree that measures nothing at construction", {
  # No device is open at all here. If the constructor measured text, this line
  # would open one (and write Rplots.pdf, which CRAN forbids).
  g <- grob_of(matrix(1:6, nrow = 2))

  expect_s3_class(g, "paintr_grob")
  expect_s3_class(g, "gTree")
  expect_true(grid::is.grob(g))
  # Children are made at DRAW time, not now.
  expect_length(g$children, 0L)

  # The fields the renderer needs, and the warn-once flag.
  expect_true(is.data.frame(g$cells))
  expect_type(g$col_w, "double")
  expect_type(g$n_row, "integer")
  expect_true(is.environment(g$state))
  expect_false(g$state$warned)

  # No font size anywhere: it cannot exist yet, because the panel does not.
  expect_false("fontsize" %in% names(g$cells))
})

test_that("paintr_grob() validates its inputs", {
  f <- fx(matrix(1:6, nrow = 2))
  expect_error(paintr_grob(list(), f$col_w, f$n_row), "cell table")
  expect_error(paintr_grob(f$cells[0, ], f$col_w, f$n_row), "cell table")
  expect_error(paintr_grob(f$cells, f$col_w[1L], f$n_row), "widths")
  expect_error(paintr_grob(f$cells, f$col_w, 0), "positive")
})

# ---------------------------------------------------------------------------
# THE test: the size is fitted at DRAW time, and it is monotone in the device
# ---------------------------------------------------------------------------

test_that("autofit is monotone in device size", {
  # warn_floor = FALSE: at 3in this grid IS below the floor and says so. That is
  # a separate test; here it would just be noise.
  g <- dense(warn_floor = FALSE)

  fs <- list()
  for (d in c(3, 7, 14)) {
    fs[[as.character(d)]] <- with_panel(d, {
      kids <- grid::makeContent(g)$children
      sig_fontsize(kids)
    })
  }

  expect_length(fs, 3L)
  expect_true(all(vapply(fs, is.finite, logical(1))))
  expect_true(all(diff(unlist(fs)) >= 0))
  # Non-vacuous: this grid is dense enough that the size genuinely GROWS, so a
  # renderer that ignored the panel and used a constant would fail here.
  expect_true(all(diff(unlist(fs)) > 0))

  # One object, three devices, three different sizes. That is the whole claim:
  # the size is an output of draw time, not a value baked into the grob.
  expect_gt(fs[["14"]], fs[["3"]])
})

test_that("the same grob object refits on redraw and is never mutated", {
  g <- dense(warn_floor = FALSE)
  small <- with_panel(3, sig_fontsize(grid::makeContent(g)$children))
  big <- with_panel(14, sig_fontsize(grid::makeContent(g)$children))
  small_again <- with_panel(3, sig_fontsize(grid::makeContent(g)$children))

  expect_gt(big, small)
  # Idempotent: drawing large did not leave a fitted size behind in the grob.
  expect_equal(small_again, small)
  expect_length(g$children, 0L)
})

test_that("a pinned fontsize is obeyed on every device", {
  g <- grob_of(matrix(1:600, nrow = 30, ncol = 20), show_all = TRUE,
               opts = paint_opts(fontsize = 8), warn_floor = FALSE)
  for (d in c(3, 7, 14)) {
    expect_equal(with_panel(d, sig_fontsize(grid::makeContent(g)$children)), 8)
  }
})

# ---------------------------------------------------------------------------
# the children: rects, and ONE text grob per span
# ---------------------------------------------------------------------------

test_that("makeContent() emits rects plus one textGrob per span", {
  # The grey/black ink assertions below name the stable classic tokens, so pin
  # the palette to classic rather than track the default palette's hexes.
  withr::local_options(paintr.palette = "classic")
  g <- grob_of(matrix(c(123456.789, 1 / 3, 100000, 0.5), nrow = 2))
  kids <- with_panel(7, grid::makeContent(g)$children)

  expect_equal(
    sort(unname(vapply(kids, function(z) z$name, character(1)))),
    c("paintr.insig", "paintr.rect", "paintr.sig")
  )
  expect_s3_class(kid(kids, "paintr.rect"), "rect")
  expect_s3_class(kid(kids, "paintr.sig"), "text")
  expect_s3_class(kid(kids, "paintr.insig"), "text")

  # Two text calls for the WHOLE plot, not two per cell.
  expect_length(kids, 3L)

  s <- kid(kids, "paintr.sig")
  i <- kid(kids, "paintr.insig")
  # 4 value cells, but only TWO grey spans. The tokens are "123457." / "0.333" /
  # "100000" / "0.5", and only the first and third carry digits past the third
  # significant one. `0.333` is exactly 3 significant digits and `0.5` is one, so
  # both are entirely black -- and the grey grob must not invent a span for them.
  expect_equal(length(s$label), 4L)
  expect_equal(length(i$label), 2L)
  expect_true(all(nzchar(i$label)))
  expect_equal(sort(i$label), c("000", "457."))
  expect_equal(paste0(s$label[1L], i$label[1L]), "123457.")

  # Left-anchored, both spans. This is the two-tone mechanism.
  expect_equal(s$hjust, 0)
  expect_equal(i$hjust, 0)
  expect_equal(s$vjust, 0.5)

  # The grey really is grey, and the black really is the cell's ink.
  expect_equal(unique(i$gp$col), "grey70")
  expect_equal(unique(s$gp$col), "black")
  expect_equal(unique(s$gp$fontfamily), "mono")
})

test_that("both backends put the cell index the same distance below its value", {
  # THE DRIFT GUARD. The nudge that separates a `[i, j]` index from the value it
  # names is DATA -- `dy_rel` on the cell table -- and `paint_resolve()` folds it
  # into the cell's `y` before either renderer ever sees it. So neither renderer
  # knows what a "cellindex" is, and neither can drift from the other.
  #
  # This test reads the offset back out of what each backend ACTUALLY EMITS -- the
  # grid textGrob's own `y` and `fontsize`, the base renderer's own resolved cells
  # -- and not out of the shared function that computed them. If someone ever
  # "fixes" this by branching on `kind` inside one renderer, this is what fails.
  #
  # The offset is compared in ROW HEIGHTS, not inches: the two backends measure
  # different panels (base reserves a title band, grid does not), so the inch
  # figures legitimately differ while the fraction of a row must not.
  #
  # AND IT ASSERTS THE INK, NOT JUST THE ANCHOR. A 3x3 -- which is all this guard
  # used to fixture -- is one of the few sizes at which the value happens to clear
  # the index anyway. From 5x5 up the value was drawn straight through the index
  # while every anchor assertion here stayed green. So the sizes below are the ones
  # that BREAK, and the claim is that the two spans' ink does not touch.
  for (case in list(
    list(matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3), show_indices = "cell"),
    list(matrix(1:36, nrow = 6), show_indices = "cell"),
    list(matrix(1:400, nrow = 20), show_indices = "cell", show_all = TRUE),
    list(1:20, show_indices = "inside")
  )) {
    f <- do.call(fx, case)
    opts <- paint_opts(family = "mono")
    n_idx <- sum(f$cells$kind == "cellindex")
    expect_gt(n_idx, 0L)

    # -- grid: read the y's off the drawn children ----------------------------
    # Every unit conversion happens INSIDE `with_panel()`, while its device is
    # still open. A `convertY()` out here, after the device has closed, would make
    # grid open the default one -- and that writes an Rplots.pdf into the working
    # directory, which is a CRAN check failure.
    g <- paintr_grob(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
    got <- with_panel(7, {
      kids <- grid::makeContent(g)$children

      txt <- kid(kids, "paintr.sig")
      y_in <- grid::convertY(txt$y, "in", valueOnly = TRUE)
      # These fixtures draw no index OUTSIDE the block, so a leading "[" is an
      # index inside a cell and nothing else.
      is_idx <- grepl("^\\[", txt$label)
      expect_equal(sum(is_idx), n_idx)
      expect_equal(sum(!is_idx), n_idx)

      # One layout unit -- one row -- in inches. A value cell is exactly one row
      # tall, and it is the shortest box drawn, so the smallest rect height IS `u`.
      # (The tallest is the outline, which spans the whole block.)
      rects <- kid(kids, "paintr.rect")
      u_grid <- min(grid::convertHeight(rects$height, "in", valueOnly = TRUE))

      # The drawn height of each span, at the size the grob actually emitted.
      hm <- measure_grid("mono")
      h_in <- vapply(
        txt$gp$fontsize,
        function(p) hm$h("Mg", p)[[1L]],
        numeric(1)
      )

      # Both chunks are built over the same grid in the same order, so the two
      # halves line up element by element.
      list(
        off = (y_in[is_idx] - y_in[!is_idx]) / u_grid,
        # The INK: the value's bottom edge against the index's top edge.
        gap = (y_in[!is_idx] - h_in[!is_idx] / 2) - (y_in[is_idx] + h_in[is_idx] / 2)
      )
    })

    # -- base: read the y's off the resolved table it drew from ----------------
    base_of <- function() {
      grDevices::pdf(NULL, width = 7, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)
      r <- render_base(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
      cs <- r$cells
      idx <- cs[cs$kind == "cellindex", , drop = FALSE]
      val <- cs[cs$kind == "value", , drop = FALSE]
      k <- match(paste(idx$i, idx$j), paste(val$i, val$j))
      val <- val[k, , drop = FALSE]
      hm <- measure_base("mono")
      h <- function(pt) vapply(pt, function(p) hm$h("Mg", p)[[1L]], numeric(1))
      list(
        off = (idx$y - val$y) / r$u,
        gap = (val$y - h(val$fontsize) / 2) - (idx$y + h(idx$fontsize) / 2)
      )
    }
    base <- base_of()

    lab <- paste0(nrow(f$cells), " cells")
    # Below, in both. And by the same amount, in both.
    expect_true(all(got$off < 0), info = lab)
    expect_true(all(base$off < 0), info = lab)
    expect_equal(base$off, got$off, tolerance = 1e-9)
    expect_equal(got$off, rep(cellindex_dy, n_idx), tolerance = 1e-9)

    # And in NEITHER is the value stamped through the index it names.
    expect_true(
      all(got$gap > 0),
      info = paste("grid", lab, "worst ink gap", signif(min(got$gap), 3), "in")
    )
    expect_true(
      all(base$gap > 0),
      info = paste("base", lab, "worst ink gap", signif(min(base$gap), 3), "in")
    )
  }
})

test_that("both backends stroke the outline at the SAME weight, and heavier", {
  # THE OTHER DRIFT GUARD, and unlike the one above it shipped BROKEN -- because
  # nothing here asserted it.
  #
  # `draw_base()` used to pull the outline out by its `kind` and draw it in a second
  # `rect()` call at a hard-coded `lwd = 2`. `paintr_children()` knew nothing about
  # that: it folded the outline in with the ordinary cell borders and never set `lwd`
  # at all. Measured on svglite, base emitted nine rects at stroke-width 0.75 and ONE
  # at 1.50, and ggplot2 emitted ten at 0.75 -- the heavy border that makes the block
  # read as one object was absent from every `gpaint_*()` picture.
  #
  # The weight is DATA now: an `lwd` column, set once in `paint_cells()`, passed
  # through by both renderers without either asking what a cell IS.
  #
  # This reads the weight back out of what each backend ACTUALLY EMITS -- the `lwd`
  # base hands `rect()`, the `lwd` grid puts in the rectGrob's `gp` -- and NOT out of
  # the shared cell table that computed it. Deleting the weight from EITHER renderer
  # must fail this test; reading the table would catch neither.
  for (case in list(
    list(matrix(1:9, nrow = 3)),
    list(matrix(1:900, nrow = 30), show_indices = "all"),
    list(iris),
    list(1:5, show_indices = "outside")
  )) {
    f <- do.call(fx, case)
    # The outline is an ordinary rect under `classic`; a refined palette rounds it
    # into a polygon, which the rounded-corner tests cover on their own.
    opts <- paint_opts(family = "mono", palette = "classic")
    lab <- paste(class(case[[1L]])[[1L]], nrow(f$cells), "cells")

    # -- base: the arguments that reached graphics::rect() ---------------------
    b <- base_rects(f, opts)
    expect_false(is.null(b$lwd), info = paste("base passed no lwd at all:", lab))
    b_lwd <- rep_len(b$lwd, length(b$xleft))
    b_area <- abs((b$xright - b$xleft) * (b$ytop - b$ybottom))

    # -- grid: the gp that reached the rectGrob --------------------------------
    g <- paintr_grob(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
    got <- with_panel(7, {
      r <- kid(grid::makeContent(g)$children, "paintr.rect")
      list(
        lwd = r$gp$lwd,
        n = length(r$x),
        area = grid::convertWidth(r$width, "in", valueOnly = TRUE) *
          grid::convertHeight(r$height, "in", valueOnly = TRUE)
      )
    })
    expect_false(is.null(got$lwd), info = paste("the rectGrob's gp carries no lwd:", lab))
    g_lwd <- rep_len(got$lwd, got$n)

    # The same rectangles, in both.
    expect_equal(length(b_lwd), length(g_lwd), info = lab)

    # Exactly ONE rectangle is stroked heavily, in each backend, and it is the one
    # that spans the whole block -- the largest box drawn. That is the outline,
    # identified by its geometry rather than by its name, because a renderer is not
    # allowed to know its name.
    heavy_b <- which(b_lwd > 1)
    heavy_g <- which(g_lwd > 1)
    expect_length(heavy_b, 1L)
    expect_length(heavy_g, 1L)
    expect_equal(heavy_b, which.max(b_area), info = lab)
    expect_equal(heavy_g, which.max(got$area), info = lab)

    # Everything else is an ordinary cell border, at weight 1.
    expect_true(all(b_lwd[-heavy_b] == 1), info = lab)
    expect_true(all(g_lwd[-heavy_g] == 1), info = lab)

    # AND THE TWO AGREE. This is the invariant: both backends draw the same picture.
    expect_equal(b_lwd[heavy_b], g_lwd[heavy_g], info = lab)
    expect_equal(b_lwd[heavy_b], outline_lwd, info = lab)
  }
})

test_that("an empty insig draws nothing at all", {
  # Scientific mode forces insig == "", so the grey textGrob must not exist.
  g <- grob_of(c(1, 1e15))
  kids <- with_panel(7, grid::makeContent(g)$children)
  expect_null(kid(kids, "paintr.insig"))
  expect_false("paintr.insig" %in% vapply(kids, function(z) z$name, character(1)))

  s <- kid(kids, "paintr.sig")
  expect_true(all(grepl("e", s$label, fixed = TRUE)))
  # ASCII only: no U+2026, no superscripts. pdf() cannot encode them.
  expect_true(all(validEnc(s$label)))
  expect_false(any(grepl("[^ -~]", s$label)))
})

test_that("the drawn geometry is the resolved geometry, in inches", {
  # Classic: every box is a rect, so `paintr.rect` carries the whole picture. The
  # rounded-corner path is covered by the corner tests.
  g <- grob_of(matrix(1:6, nrow = 2, ncol = 3), show_indices = "all",
               opts = paint_opts(palette = "classic"))

  got <- with_panel(7, {
    kids <- grid::makeContent(g)$children
    res <- paint_resolve(
      g$cells, g$col_w, g$n_row,
      panel = panel_grid(),
      measure = measure_grid(g$opts$family),
      opts = g$opts
    )
    list(kids = kids, res = res)
  })
  kids <- got$kids
  cells <- got$res$cells

  # Rects: xl/yb, left-bottom justified, straight through in inches.
  boxed <- cells[!is.na(cells$fill) | !is.na(cells$border), ]
  r <- kid(kids, "paintr.rect")
  expect_equal(sort(as.numeric(r$x)), sort(boxed$xl))
  expect_equal(sort(as.numeric(r$y)), sort(boxed$yb))
  expect_equal(sort(as.numeric(r$width)), sort(boxed$xr - boxed$xl))
  expect_equal(r$just, c("left", "bottom"))

  # The outline draws LAST, so a value cell's border cannot paint over it.
  expect_equal(sum(is.na(r$gp$fill)), 1L)
  expect_true(is.na(r$gp$fill[[length(r$gp$fill)]]))

  # Text: the span sits at the cell centre PLUS its own dx, in inches.
  sig <- cells[nzchar(cells$sig), ]
  s <- kid(kids, "paintr.sig")
  expect_equal(as.numeric(s$x), sig$x + sig$dx_sig)
  expect_equal(as.numeric(s$y), sig$y)
  expect_equal(s$gp$fontsize, sig$fontsize)
  # size_rel is live: the grey index labels are drawn smaller than the values.
  expect_gt(length(unique(s$gp$fontsize)), 1L)
})

test_that("nothing the grob draws escapes the panel", {
  g <- dense(warn_floor = FALSE)
  for (d in c(3, 7, 14)) {
    with_panel(d, {
      kids <- grid::makeContent(g)$children
      r <- kid(kids, "paintr.rect")
      xl <- as.numeric(r$x)
      yb <- as.numeric(r$y)
      expect_gte(min(xl), -1e-9)
      expect_gte(min(yb), -1e-9)
      expect_lte(max(xl + as.numeric(r$width)), d + 1e-9)
      expect_lte(max(yb + as.numeric(r$height)), d + 1e-9)
    })
  }
})

# ---------------------------------------------------------------------------
# the floor warning: at DRAW time, with the REAL device, exactly once
# ---------------------------------------------------------------------------

test_that("the floor warning fires EXACTLY ONCE across two makeContent() calls", {
  g <- dense()
  expect_false(g$state$warned)

  with_panel(3, {
    # First draw: the device is real, it is small, and 600 cells cannot be legible.
    expect_warning(grid::makeContent(g), "legibility floor")
    # Every subsequent draw -- a resize, a redraw, a ggsave() replay -- is silent.
    expect_no_warning(grid::makeContent(g))
    expect_no_warning(grid::makeContent(g))
  })

  expect_true(g$state$warned)

  # A fresh grob for the same data warns again: the flag is per OBJECT.
  h <- dense()
  with_panel(3, expect_warning(grid::makeContent(h), "legibility floor"))
})

test_that("the warning quotes the REAL device, and stays silent when it is big enough", {
  g <- dense()
  # 3in: floored. This is the case that a construction-time warning against a
  # fabricated reference device would have MISSED.
  w <- with_panel(3, tryCatch(grid::makeContent(g), warning = function(e) conditionMessage(e)))
  expect_type(w, "character")
  expect_match(w, "600 cells")
  expect_match(w, "5.0 pt legibility floor")
  expect_match(w, "Enlarge the device")
  expect_match(w, "paintr.warn_floor")
  # ASCII only -- this string can end up in an R CMD check log on a latin1 box.
  expect_false(grepl("[^ -~\n]", w))

  # And the suggested size is not a guess: at that device the fit clears the floor.
  need <- as.numeric(regmatches(w, regexpr("[0-9.]+ x [0-9.]+", w)) |>
                       strsplit(" x ") |> unlist())
  expect_length(need, 2L)
  big <- dense()
  fs <- with_panel(need[[1L]], sig_fontsize(grid::makeContent(big)$children))
  expect_gte(fs, paint_opts()$min_pt)
  expect_false(big$state$warned)

  # 14in: not floored, and a grob that warns here would be crying wolf.
  h <- dense()
  with_panel(14, expect_no_warning(grid::makeContent(h)))
  expect_false(h$state$warned)
})

test_that("the warning is gated, by the field and by the option", {
  # Threaded down as plain data by the painter.
  g <- grob_of(matrix(1:600, nrow = 30, ncol = 20), show_all = TRUE, warn_floor = FALSE)
  with_panel(3, expect_no_warning(grid::makeContent(g)))

  # And re-read at draw time as a live kill switch: a ggplot object is built long
  # before it is drawn, so the option must still bite in between.
  h <- dense()
  withr::local_options(paintr.warn_floor = FALSE)
  with_panel(3, expect_no_warning(grid::makeContent(h)))
})

test_that("min_pt is a threshold, not a clamp: the honest small size is drawn", {
  g <- dense(warn_floor = FALSE)
  fs <- with_panel(3, sig_fontsize(grid::makeContent(g)$children))
  opts <- paint_opts()
  expect_lt(fs, opts$min_pt)
  # The specific bug: `max(fs, min_pt)` would have produced exactly 5, and the
  # digits would overlap.
  expect_false(isTRUE(all.equal(fs, opts$min_pt)))
})

# ---------------------------------------------------------------------------
# the whole point of the file: no ggplot2, anywhere
# ---------------------------------------------------------------------------

test_that("drawing the grob loads no ggplot2 and needs no Suggests", {
  skip_if("ggplot2" %in% loadedNamespaces())
  g <- grob_of(iris)
  with_panel(7, expect_no_error(grid::grid.draw(grid::makeContent(g))))
  expect_false("ggplot2" %in% loadedNamespaces())
})

test_that("grid.draw() dispatches makeContent.paintr_grob() through S3 registration", {
  # Not the same claim as calling makeContent() by hand: this proves the method
  # is REGISTERED, which is what grid's drawing engine relies on.
  g <- grob_of(matrix(c(1.5, 1 / 3, 2, 3), nrow = 2))
  with_panel(7, {
    expect_no_error(grid::grid.draw(g))
    # forceGrob() runs makeContent() the way the engine does.
    forced <- grid::forceGrob(g)
    expect_gt(length(grid::childNames(forced)), 0L)
  })
})

# ---------------------------------------------------------------------------
# every structure, on the grid path
# ---------------------------------------------------------------------------

test_that("every structure draws through the grob without error or NA", {
  for (data in list(
    1:3,
    c(1.5, 2),
    letters,
    c(TRUE, FALSE, NA),
    factor(c("a", "b")),
    Sys.Date() + 0:2,
    c(NA, NaN, Inf, -Inf),
    c(1, 1e15),
    matrix(letters[1:6], nrow = 2),
    matrix(1:900, nrow = 30, ncol = 30),
    iris,
    data.frame(x = 1:2, y = I(list(1:3, "z")))
  )) {
    g <- grob_of(data, warn_floor = FALSE)
    with_panel(7, {
      kids <- grid::makeContent(g)$children
      expect_gt(length(kids), 0L)
      for (k in kids) {
        expect_false(any(is.na(as.numeric(k$x))))
        expect_false(any(is.na(as.numeric(k$y))))
      }
      s <- kid(kids, "paintr.sig")
      expect_false(any(is.na(s$gp$fontsize)))
      expect_true(all(s$gp$fontsize > 0))
      expect_no_error(grid::grid.draw(g))
    })
  }
})

# ---------------------------------------------------------------------------
# the span predicate: ONE question, asked identically by both backends
# ---------------------------------------------------------------------------

# The NA-in-sig bug. The two renderers filtered their text rows with DIFFERENT
# predicates:
#
#   R/render-base.R   nzchar(cells$sig)               -- nzchar(NA) is TRUE  -> KEPT
#   R/render-grid.R   !is.na(sig) & nzchar(sig)       -- NA                  -> DROPPED
#
# They agreed only BY ACCIDENT: `graphics::text()` silently skips an NA label and draws
# nothing, so base's extra row happened to leave no ink. Nothing puts a true NA into
# `sig` today, so it was latent -- but it is a live divergence in the one invariant this
# branch is built on, BOTH BACKENDS DRAW THE SAME PICTURE, and drawing `names(x)` (where
# `names()` can hold a true NA) would make it reachable. Both files ask the same question
# now, and this test is what holds them to it.

test_that("the NA-in-sig bug: both backends drop an NA span, and drop the SAME one", {
  f <- fx(matrix(1:4, nrow = 2))

  # A value cell whose `sig` is a TRUE NA -- not the literal token "NA" that
  # `paint_format()` emits -- and whose `insig` is empty, so the row carries no ink of
  # any kind and BOTH backends must drop it.
  val <- which(f$cells$kind == "value")
  expect_length(val, 4L)
  target <- val[[2L]]
  f$cells$sig[[target]] <- NA_character_
  f$cells$insig[[target]] <- ""

  # grid: the black textGrob's labels ARE the black spans.
  g <- paintr_grob(f$cells, f$col_w, f$n_row, opts = paint_opts(), warn_floor = FALSE)
  grid_labels <- kid(with_panel(7, grid::makeContent(g)$children), "paintr.sig")$label

  # base: the labels handed to the first `text()` call ARE the black spans.
  calls <- base_text(f)
  expect_length(calls, 2L)
  base_labels <- calls[[1L]]$labels

  # The NA row is gone from BOTH -- 4 value cells, 3 spans.
  expect_length(grid_labels, 3L)
  expect_length(base_labels, 3L)

  # The same number of spans, and the SAME spans. This is the whole invariant.
  expect_identical(base_labels, grid_labels)

  # And no NA ever reaches the device: base used to rely on `text()` skipping it.
  expect_false(anyNA(base_labels))
  expect_false(anyNA(grid_labels))
  expect_identical(sort(base_labels), c("1", "3", "4"))
})

test_that("the NA-in-sig bug: an NA insig is dropped by both backends too", {
  f <- fx(matrix(1:4, nrow = 2))
  val <- which(f$cells$kind == "value")
  # A real black span, but an NA grey one. The row must still be drawn -- it has ink --
  # and the grey span must not be.
  f$cells$insig[[val[[1L]]]] <- NA_character_

  g <- paintr_grob(f$cells, f$col_w, f$n_row, opts = paint_opts(), warn_floor = FALSE)
  kids <- with_panel(7, grid::makeContent(g)$children)

  calls <- base_text(f)
  base_sig <- calls[[1L]]$labels

  # All four black spans survive in both: an NA `insig` is not a reason to drop a row.
  expect_length(kid(kids, "paintr.sig")$label, 4L)
  expect_length(base_sig, 4L)
  expect_identical(base_sig, kid(kids, "paintr.sig")$label)

  # And grid emits no grey grob at all, because no cell has a grey span.
  expect_null(kid(kids, "paintr.insig"))
})

# What `draw_base()` hands `graphics::polygon()` -- the rounded cells (block
# outline, header band) a refined palette draws. Same capture trick as
# `base_rects()`; classic draws none, so the list comes back empty.
base_polygons <- function(f, opts = paint_opts()) {
  got <- list()
  testthat::local_mocked_bindings(
    polygon = function(...) {
      got[[length(got) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "graphics"
  )
  grDevices::pdf(NULL, width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  render_base(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
  got
}

test_that("a refined palette rounds the outline into a polygon; classic is a rect", {
  f <- fx(matrix(1:9, nrow = 3))

  # -- classic: no polygon at all, and the heavy stroke is an ordinary rect --
  expect_length(base_polygons(f, paint_opts(family = "mono", palette = "classic")), 0L)
  g_c <- paintr_grob(f$cells, f$col_w, f$n_row,
    opts = paint_opts(family = "mono", palette = "classic"), warn_floor = FALSE)
  kids_c <- with_panel(7, grid::makeContent(g_c)$children)
  expect_null(kid(kids_c, "paintr.round.1"))
  expect_false(is.null(kid(kids_c, "paintr.rect")))

  # -- mint: the outline is ONE polygon, in both backends, at the outline weight --
  opts <- paint_opts(family = "mono", palette = "mint")
  polys <- base_polygons(f, opts)
  expect_length(polys, 1L)                    # a matrix has no band, so just the card
  expect_equal(polys[[1L]]$lwd, outline_lwd)
  expect_length(polys[[1L]]$x, 32L)           # four quarter-circle arcs of n = 8

  g_m <- paintr_grob(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
  round_g <- with_panel(7, Filter(
    function(z) startsWith(z$name, "paintr.round."),
    grid::makeContent(g_m)$children
  ))
  expect_length(round_g, 1L)
  expect_equal(round_g[[1L]]$gp$lwd, outline_lwd)

  # -- parity: grid traces EXACTLY the shared rounded_rect_xy of the resolved card --
  got <- with_panel(7, {
    res <- paint_resolve(f$cells, f$col_w, f$n_row,
      panel = panel_grid(), measure = measure_grid(opts$family), opts = opts)
    ol <- res$cells[!is.na(res$cells$border) & res$cells$lwd >= 1.5, ]
    exp <- rounded_rect_xy(ol$xl, ol$yb, ol$xr, ol$yt, ol$radius)
    g <- kid(grid::makeContent(g_m)$children, "paintr.round.1")
    list(exp = exp,
         gx = grid::convertX(g$x, "in", valueOnly = TRUE),
         gy = grid::convertY(g$y, "in", valueOnly = TRUE),
         radius = ol$radius)
  })
  expect_equal(got$radius, 3 / 72)            # 3pt, in inches
  expect_equal(got$gx, got$exp$x, tolerance = 1e-6)
  expect_equal(got$gy, got$exp$y, tolerance = 1e-6)
})

test_that("a styled data frame rounds BOTH the card and the header band", {
  # The band cell exists only on a styled table; the painters pass styled with a
  # non-classic palette, so pair the two here to reach the banded path.
  f <- fx(head(mtcars, 4), styled = TRUE)
  opts <- paint_opts(family = "mono", palette = "mint")
  polys <- base_polygons(f, opts)
  # Two rounded shapes: the outline (outline_lwd) and the band (floored to 1).
  lwds <- vapply(polys, function(p) p$lwd, numeric(1))
  expect_true(outline_lwd %in% lwds)
  expect_gte(length(polys), 2L)

  g <- paintr_grob(f$cells, f$col_w, f$n_row, opts = opts, warn_floor = FALSE)
  round_g <- with_panel(7, Filter(
    function(z) startsWith(z$name, "paintr.round."),
    grid::makeContent(g)$children
  ))
  expect_gte(length(round_g), 2L)
})
