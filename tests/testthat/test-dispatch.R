# The dispatch battery. This is the test that catches the `.vector` trap.
#
# R's "vector" is not the concept its name suggests, and the package has been bitten
# by it twice:
#
#   * `inherits(letters, "vector")` is FALSE, so `highlight_data.vector()` is never
#     SELECTED by UseMethod() for an atomic vector and `highlight_data(letters)`
#     fell through to `.default`, which stops. (Bug 5.)
#   * `is.vector(factor("a"))` is FALSE -- `is.vector()` rejects anything carrying an
#     attribute other than `names` -- so `paint_vector()`'s type guard rejected every
#     factor and every Date.
#
# Neither is caught by testing "does a numeric vector draw?". They are only caught by
# looping every type across every painter, which is what this file does.
#
# The data frame painters and `paint_size()` do not exist yet, so the batteries that
# cover them land with them.

# `pdf(NULL)` is the test device: full strwidth()/par() support, no file I/O, and
# no Rplots.pdf left in the working directory for R CMD check to complain about.
with_null_pdf <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

inputs <- list(
  int = 1:3,
  dbl = c(1.5, 2),
  chr = letters,
  lgl = c(TRUE, FALSE),
  fct = factor("a"),
  date = Sys.Date(),
  nonfinite = c(NA, NaN, Inf),
  cplx = complex(real = 1, imaginary = 1)
)

# ---------------------------------------------------------------------------
# every input x every applicable painter
# ---------------------------------------------------------------------------

test_that("paint_vector() draws every atomic type", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      expect_no_error(paint_vector(inputs[[nm]]), message = nm)
      expect_no_error(paint_vector(inputs[[nm]], layout = "horizontal"), message = nm)
    }
  })
})

test_that("gpaint_vector() draws every atomic type", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      # Constructed AND printed: the font fit happens inside makeContent(), at draw
      # time, so a test that only builds the object never executes the hardest code
      # in the package.
      expect_no_error(print(gpaint_vector(inputs[[nm]])), message = nm)
    }
  })
})

test_that("paint_matrix() draws every atomic type", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      m <- matrix(inputs[[nm]], ncol = 1)
      expect_no_error(paint_matrix(m), message = nm)
      expect_no_error(paint_matrix(m, show_indices = "all"), message = nm)
    }
  })
})

test_that("gpaint_matrix() draws every atomic type", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      m <- matrix(inputs[[nm]], ncol = 1)
      expect_no_error(print(gpaint_matrix(m)), message = nm)
    }
  })
})

# ---------------------------------------------------------------------------
# Bug 5: highlight_data() on the types that used to fall through to .default
# ---------------------------------------------------------------------------

test_that("highlight_data() accepts character, logical and data frames", {
  expect_no_error(highlight_data(letters))
  expect_no_error(highlight_data(c(TRUE, FALSE)))
  expect_no_error(highlight_data(iris))

  # And the result is the right shape, not merely non-erroring.
  expect_identical(highlight_data(letters), rep(FALSE, 26))
  expect_identical(dim(highlight_data(iris)), c(150L, 5L))
})

test_that("highlight_locations() feeds a character vector into paint_vector()", {
  # The exact composition the design document calls out: without the Bug 5 fix the
  # character repair ships half-broken, because this line still errors.
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    expect_no_error(
      paint_vector(letters, highlight_area = highlight_locations(letters, 2))
    )
  })
})

# ---------------------------------------------------------------------------
# BUG 1 REGRESSION: character cells must contain the ACTUAL STRINGS
# ---------------------------------------------------------------------------
#
# The old code's value/colour logic was an `ifelse` chain over is.finite() /
# is.na() / is.nan(). A string is none of those, so it fell off the end and every
# character cell came out as a red "Unknown". This asserts on the returned cell
# table, so it needs no image diffing.

test_that("a character matrix renders its strings, not 'Unknown'", {
  m <- matrix(c("apple", "banana", "cherry", "date"), nrow = 2)

  res <- with_null_pdf(paint_matrix(m))
  cells <- res$cells
  vals <- cells[cells$kind == "value", , drop = FALSE]

  # The four strings are in the table, in column-major order, exactly as given.
  expect_identical(vals$sig, c("apple", "banana", "cherry", "date"))
  expect_identical(paste0(vals$sig, vals$insig), c("apple", "banana", "cherry", "date"))

  # Not one cell anywhere says "Unknown", and not one is inked red: these strings
  # are all perfectly ordinary values.
  expect_false(any(grepl("Unknown", cells$sig, fixed = TRUE)))
  expect_false(any(grepl("Unknown", cells$insig, fixed = TRUE)))
  expect_identical(unique(vals$ink), "black")
  expect_identical(unique(vals$align), "left")
})

test_that("a character vector renders its strings, not 'Unknown'", {
  res <- with_null_pdf(paint_vector(c("alpha", "beta", NA)))
  vals <- res$cells[res$cells$kind == "value", , drop = FALSE]

  expect_identical(vals$sig, c("alpha", "beta", "NA"))
  expect_false(any(grepl("Unknown", res$cells$sig, fixed = TRUE)))
  # NA is the one genuinely missing value, and it is the only red cell.
  expect_identical(vals$ink, c("black", "black", "red"))
})

test_that("numbers are not rendered at 15 significant digits (bug 2)", {
  res <- with_null_pdf(paint_vector(c(1 / 3, pi)))
  vals <- res$cells[res$cells$kind == "value", , drop = FALSE]

  tok <- paste0(vals$sig, vals$insig)
  expect_identical(tok, c("0.333", "3.14"))
  # The black span stops at three significant digits.
  expect_identical(vals$sig, c("0.333", "3.14"))
})

# ---------------------------------------------------------------------------
# error handling
# ---------------------------------------------------------------------------

test_that("the matrix painter's type error names the matrix type (bug 7)", {
  expect_error(paint_matrix(1:3), "`matrix` type")
  expect_error(paint_matrix(iris), "`matrix` type")
  skip_if_not_installed("ggplot2")
  expect_error(gpaint_matrix(1:3), "`matrix` type")
})

test_that("the painters reject the wrong shape", {
  expect_error(paint_vector(matrix(1:4, 2)), "`vector` type")
  expect_error(paint_vector(iris), "`vector` type")
})

test_that("empty data stops", {
  expect_error(paint_vector(numeric(0)), "empty")
  expect_error(paint_matrix(matrix(numeric(0), nrow = 0, ncol = 0)), "empty")
})

test_that("show_indices is match.arg'd", {
  expect_error(paint_matrix(matrix(1:4, 2), show_indices = "wombat"), "should be one of")
  expect_error(paint_vector(1:3, show_indices = "cell"), "should be one of")
  # "inside"/"outside" are the vector's vocabulary, "cell"/"row"/"column" the grid's.
  expect_error(paint_matrix(matrix(1:4, 2), show_indices = "inside"), "should be one of")
})

test_that("a mis-shaped highlight_area reports the actual dimensions", {
  m <- matrix(1:6, nrow = 2)
  expect_error(
    paint_matrix(m, highlight_area = matrix(TRUE, nrow = 3, ncol = 3)),
    "2 by 3 logical matrix.*but it is 3 by 3"
  )
  expect_error(
    paint_vector(1:3, highlight_area = c(TRUE, FALSE)),
    "must have 3 values"
  )
})

test_that("highlight_area = NULL highlights nothing and a length-1 logical recycles", {
  m <- matrix(1:4, nrow = 2)

  none <- with_null_pdf(paint_matrix(m))
  vals <- none$cells[none$cells$kind == "value", , drop = FALSE]
  expect_identical(unique(vals$fill), "white")

  # The documented `gpaint_matrix(mat, highlight_area = FALSE)` example.
  all_f <- with_null_pdf(paint_matrix(m, highlight_area = FALSE))
  vals_f <- all_f$cells[all_f$cells$kind == "value", , drop = FALSE]
  expect_identical(unique(vals_f$fill), "white")

  all_t <- with_null_pdf(paint_matrix(m, highlight_area = TRUE))
  vals_t <- all_t$cells[all_t$cells$kind == "value", , drop = FALSE]
  expect_identical(unique(vals_t$fill), "lemonchiffon")

  # NA in the mask reads as FALSE, which is the current behaviour.
  na_mask <- matrix(c(TRUE, NA, FALSE, NA), nrow = 2)
  res <- with_null_pdf(paint_matrix(m, highlight_area = na_mask))
  vals_na <- res$cells[res$cells$kind == "value", , drop = FALSE]
  expect_identical(vals_na$fill, c("lemonchiffon", "white", "white", "white"))
})

test_that("sigfig outside 1:15 stops", {
  expect_error(paint_vector(1:3, sigfig = 0), "between 1 and 15")
  expect_error(paint_vector(1:3, sigfig = 16), "between 1 and 15")
  expect_error(paint_matrix(matrix(1:4, 2), sigfig = 99), "between 1 and 15")
})

test_that("the hard 1e5 cell ceiling stops, even under show_all", {
  big <- matrix(0, nrow = 1000, ncol = 101)
  expect_error(paint_matrix(big), "100000 cells")
  expect_error(paint_matrix(big, show_all = TRUE), "100000 cells")
})

# ---------------------------------------------------------------------------
# options are read at the top of the painter, never in a formal default
# ---------------------------------------------------------------------------

test_that("paintr.ellipsis is honoured and is not a formal default", {
  # Not in any formal: if it were, the option would be baked in when the package was
  # built, and changing it at run time would do nothing.
  expect_false("ellipsis" %in% names(formals(paint_matrix)))
  expect_false("ellipsis" %in% names(formals(paint_vector)))
  expect_false("warn_floor" %in% names(formals(paint_matrix)))

  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  # The default is ASCII. pdf() cannot encode U+2026, and pdf() is the device
  # R CMD check uses.
  res <- with_null_pdf(paint_vector(1:30))
  expect_true(any(res$cells$sig == "..."))

  op <- options(paintr.ellipsis = "~~")
  on.exit(options(op), add = TRUE)
  res2 <- with_null_pdf(paint_vector(1:30))
  expect_true(any(res2$cells$sig == "~~"))
  expect_false(any(res2$cells$sig == "..."))
})

# ---------------------------------------------------------------------------
# elision and the note
# ---------------------------------------------------------------------------

test_that("a long structure elides its middle and carries a note", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  res <- with_null_pdf(paint_matrix(matrix(1:900, nrow = 30), show_indices = "all"))
  cells <- res$cells
  # 30 rows, 30 columns; max_rows = 20, max_cols = 15: 19 kept rows, 14 kept cols
  # (one drawn lane in each direction is spent on the "..." gap).
  expect_equal(sum(cells$kind == "value"), 19L * 14L)
  expect_true(any(cells$kind == "ellipsis"))

  # Original indices survive elision: the last row is still labelled [30, ].
  labs <- cells$sig[cells$kind == "rowlabel"]
  expect_true("[30, ]" %in% labs)
  expect_true("[, 30]" %in% cells$sig[cells$kind == "collabel"])
})

test_that("show_all = TRUE draws every cell", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  res <- with_null_pdf(paint_matrix(matrix(1:900, nrow = 30), show_all = TRUE))
  expect_equal(sum(res$cells$kind == "value"), 900L)
  expect_false(any(res$cells$kind == "ellipsis"))
})

# ---------------------------------------------------------------------------
# min_pt is a threshold, never a clamp
# ---------------------------------------------------------------------------

test_that("show_all on a small device renders below the floor and warns once", {
  # If min_pt were clamped UP -- `max(fs, min_pt)` -- the text would overlap, which
  # is the exact smear this engine exists to fix. So the honest small size is drawn
  # and the user is warned.
  grDevices::pdf(NULL, width = 3, height = 3)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_warning(
    res <- paint_matrix(matrix(1:900, nrow = 30), show_all = TRUE),
    "legibility floor"
  )
  expect_true(res$floored)
  expect_lt(res$fontsize, 5)

  # And it can be silenced.
  op <- options(paintr.warn_floor = FALSE)
  on.exit(options(op), add = TRUE)
  expect_no_warning(paint_matrix(matrix(1:900, nrow = 30), show_all = TRUE))
})

# ---------------------------------------------------------------------------
# a matrix is ONE formatting unit
# ---------------------------------------------------------------------------

test_that("a matrix is ONE formatting unit: one 1e15 flips the whole thing", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  m <- matrix(c(1, 2, 3, 1e15), nrow = 2)
  res <- with_null_pdf(paint_matrix(m))
  vals <- res$cells[res$cells$kind == "value", , drop = FALSE]

  expect_identical(unique(vals$fmt_group), 1L)
  expect_identical(vals$sig, c("1.00e+00", "2.00e+00", "3.00e+00", "1.00e+15"))
  # Hard rule: in scientific mode insig is "" -- the exponent is fully significant.
  expect_identical(unique(vals$insig), "")
})

# ---------------------------------------------------------------------------
# ggplot2 skin
# ---------------------------------------------------------------------------

test_that("gpaint_* returns a real ggplot that survives + theme() and ggsave()", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  g <- gpaint_matrix(matrix(1:4, nrow = 2))
  expect_s3_class(g, "ggplot")
  expect_no_error(g + ggplot2::theme(plot.margin = ggplot2::unit(rep(1, 4), "pt")))

  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(
    ggplot2::ggsave(f, g, width = 5, height = 5, device = grDevices::pdf)
  )
  expect_true(file.exists(f))
})

test_that("the grob refits when the device changes size (deferred sizing)", {
  # The claim the whole custom-grob design rests on: the SAME object draws at a
  # different font size on a different device. If the size were baked into the cell
  # table at build time, these two would be equal.
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  # A 20x20, not a 3x3: a 3x3 is so roomy that it hits paint_opts()'s 24pt max_pt
  # cap at every device size, and a capped size is constant, which would make this
  # test pass for the wrong reason (it would also pass on a build-time-baked size).
  cells <- paintr:::paint_cells(matrix(1:400, nrow = 20))
  col_w <- paintr:::column_widths(cells)
  n_row <- attr(cells, "n_row")

  fit_at <- function(w, h) {
    grDevices::pdf(NULL, width = w, height = h)
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(width = grid::unit(w, "in"), height = grid::unit(h, "in")))
    g <- paintr:::paintr_grob(cells, col_w, n_row, warn_floor = FALSE)
    grid::makeContent(g)$children[["paintr.sig"]]$gp$fontsize[[1L]]
  }

  small <- fit_at(3, 3)
  big <- fit_at(7, 7)
  huge <- fit_at(14, 14)

  # Strictly monotone in the device size. If the size were baked into the cell table
  # at build time, all three would be equal.
  expect_lt(small, big)
  expect_lt(big, huge)
  # ... and it never runs away: 24pt is paint_opts()'s max_pt cap.
  expect_lte(huge, 24)

  # NEVER snapshot a computed font size: pdf() quantizes it to integer points (a
  # requested 3.6pt renders at 4pt). Monotonicity is the reproducible claim.
})

# ---------------------------------------------------------------------------
# the subtitle
# ---------------------------------------------------------------------------
#
# Three states, and the middle one is the reason the default cannot simply live in
# the formals: `NULL` has to mean "compute it", so "draw nothing" needs a value of
# its own, and that value is `NA`.
#
#   graph_subtitle = NULL   -> the computed line
#   graph_subtitle = NA     -> no subtitle at all
#   graph_subtitle = "text" -> exactly that

test_that("the default subtitle describes the data", {
  with_null_pdf({
    expect_equal(
      paint_matrix(matrix(1:6, nrow = 2))$graph_subtitle,
      "Dimensions: 2 rows x 3 columns | Data Type: matrix, array"
    )
    expect_equal(
      paint_vector(c(1, 2, 3, 4, 5))$graph_subtitle,
      "Length: 5 elements | Data Type: numeric"
    )
    expect_equal(
      paint_vector(letters[1:3])$graph_subtitle,
      "Length: 3 elements | Data Type: character"
    )
  })
})

test_that("graph_subtitle = NA suppresses it, and a string is drawn verbatim", {
  with_null_pdf({
    m <- matrix(1:6, nrow = 2)

    expect_null(paint_matrix(m, graph_subtitle = NA)$graph_subtitle)
    expect_null(paint_matrix(m, graph_subtitle = "")$graph_subtitle)
    expect_null(paint_vector(1:5, graph_subtitle = NA)$graph_subtitle)

    expect_equal(
      paint_matrix(m, graph_subtitle = "a line of my own")$graph_subtitle,
      "a line of my own"
    )
    expect_equal(
      paint_vector(1:5, graph_subtitle = "mine")$graph_subtitle,
      "mine"
    )
  })
})

test_that("the subtitle reports the ORIGINAL shape of an elided structure", {
  # The subtitle describes the DATA. The "# N more rows" note describes the
  # DRAWING. A 30-row matrix drawn as 20 rows still HAS 30 rows, and a subtitle
  # read off the cell table -- which knows only the drawn extent -- would say 20
  # and quietly lose the fact the user most needs.
  with_null_pdf({
    r <- paint_matrix(matrix(seq_len(90), nrow = 30, ncol = 3))
    expect_equal(
      r$graph_subtitle,
      "Dimensions: 30 rows x 3 columns | Data Type: matrix, array"
    )
    # It really was elided: fewer rows drawn than the subtitle reports, and a note
    # saying so.
    expect_lt(max(r$cells$row), 30L)
    expect_match(r$note, "more rows")

    rv <- paint_vector(seq_len(50))
    expect_equal(rv$graph_subtitle, "Length: 50 elements | Data Type: integer")
    expect_lt(max(rv$cells$row), 50L)
    expect_match(rv$note, "more rows")
  })
})

test_that("gpaint_* carries the same subtitle contract", {
  # Both backends or neither: a subtitle that only the base painter draws is the
  # same class of drift as a nudge that only one renderer honours.
  skip_if_not_installed("ggplot2")
  m <- matrix(1:6, nrow = 2)

  expect_equal(
    gpaint_matrix(m)$labels$subtitle,
    "Dimensions: 2 rows x 3 columns | Data Type: matrix, array"
  )
  expect_equal(
    gpaint_vector(1:5)$labels$subtitle,
    "Length: 5 elements | Data Type: integer"
  )
  expect_null(gpaint_matrix(m, graph_subtitle = NA)$labels$subtitle)
  expect_equal(gpaint_matrix(m, graph_subtitle = "mine")$labels$subtitle, "mine")
})

# ---------------------------------------------------------------------------
# Bug 6: par() is restored
# ---------------------------------------------------------------------------

test_that("every base painter restores par()", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  # par() is per-device, so this must be measured on a LIVE device: closing it
  # resets the evidence.
  before <- graphics::par(no.readonly = TRUE)

  paint_matrix(matrix(1:4, nrow = 2))
  expect_equal(graphics::par(no.readonly = TRUE), before)

  paint_vector(1:4)
  expect_equal(graphics::par(no.readonly = TRUE), before)
})
