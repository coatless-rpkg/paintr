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
  g <- grob_of(matrix(1:6, nrow = 2, ncol = 3), show_indices = "all")

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
