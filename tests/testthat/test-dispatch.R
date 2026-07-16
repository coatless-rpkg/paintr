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

test_that("paint_data_frame() draws every atomic type", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      df <- data.frame(x = inputs[[nm]])
      expect_no_error(paint_data_frame(df), message = nm)
      expect_no_error(paint_df(df, show_types = FALSE), message = nm)
    }
  })
})

test_that("gpaint_data_frame() draws every atomic type", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      df <- data.frame(x = inputs[[nm]])
      expect_no_error(print(gpaint_data_frame(df)), message = nm)
      expect_no_error(print(gpaint_df(df)), message = nm)
    }
  })
})

test_that("paint_list() draws every atomic type, as an element", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      l <- list(x = inputs[[nm]], y = 1:2)
      expect_no_error(paint_list(l), message = nm)
      expect_no_error(paint_list(l, summarise = TRUE), message = nm)
      expect_no_error(paint_list(l, show_indices = "cell"), message = nm)
    }
  })
})

test_that("gpaint_list() draws every atomic type, as an element", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  with_null_pdf({
    for (nm in names(inputs)) {
      expect_no_error(print(gpaint_list(list(x = inputs[[nm]], y = 1:2))), message = nm)
    }
  })
})

test_that("THE TYPE GATE: every list-backed S3 class is refused, by painter and by mask", {
  # `is.list()` is TRUE for every one of these, and a naive gate would draw
  # as.POSIXlt(Sys.time()) as ELEVEN ragged columns of sec/min/hour/mday/... -- a
  # wrong picture of a datetime, drawn confidently, with no error. It is the same
  # family as `inherits(1:3, "vector")` being FALSE, which this package has already
  # been bitten by twice.
  #
  # THE PAINTER AND THE MASK MUST AGREE ABOUT WHAT A LIST IS, or one of them draws
  # what the other cannot describe. They are asserted together, here, on one battery.
  classed <- list(
    data.frame = data.frame(a = 1:2),
    POSIXlt    = as.POSIXlt(Sys.time()),
    lm         = stats::lm(mpg ~ cyl, mtcars),
    htest      = stats::t.test(1:10),
    by         = by(warpbreaks[, 1:2], warpbreaks[, "tension"], summary)
  )

  with_null_pdf({
    for (nm in names(classed)) {
      expect_true(is.list(classed[[nm]]), info = nm)
      expect_false(is_paint_list(classed[[nm]]), info = nm)
      expect_error(paint_list(classed[[nm]]), "`list` type", info = nm)
      expect_error(gpaint_list(classed[[nm]]), "`list` type", info = nm)
    }
    # A data frame is masked (it is painted, by paint_data_frame()); the rest are not.
    expect_no_error(highlight_data(classed$data.frame, columns = 1))
    for (nm in setdiff(names(classed), "data.frame")) {
      expect_error(highlight_data(classed[[nm]], rows = 1), "do not support", info = nm)
    }
    # A tibble is a data frame all the way down, and both halves agree about that.
    # Built here rather than depended on: a tibble IS this object, and what is under
    # test is the class vector, not the package.
    tb <- structure(
      list(a = 1:2),
      class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -2L)
    )
    expect_false(is_paint_list(tb))
    expect_error(paint_list(tb), "`list` type")
    expect_no_error(paint_data_frame(tb))
    expect_no_error(highlight_data(tb, columns = 1))
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
  # Assert on the stable classic ink tokens, not the active palette's hexes.
  withr::local_options(paintr.palette = "classic")
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
  # Assert on the stable classic ink tokens, not the active palette's hexes.
  withr::local_options(paintr.palette = "classic")
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
  expect_error(paint_data_frame(1:3), "`data.frame` type")
  expect_error(paint_data_frame(matrix(1:4, 2)), "`data.frame` type")
})

test_that("empty data stops", {
  expect_error(paint_vector(numeric(0)), "empty")
  expect_error(paint_matrix(matrix(numeric(0), nrow = 0, ncol = 0)), "empty")
  expect_error(paint_data_frame(data.frame()), "empty")
})

test_that("show_indices rejects a value outside its structure's vocabulary", {
  # A grid takes a vector, so it is validated by hand, not by match.arg().
  expect_error(paint_matrix(matrix(1:4, 2), show_indices = "wombat"), "must be one or more of")
  # "inside"/"outside" are the vector's vocabulary, "cell"/"row"/"column" the grid's.
  expect_error(paint_matrix(matrix(1:4, 2), show_indices = "inside"), "must be one or more of")
  # A vector's placements are mutually exclusive, so it is still match.arg'd.
  expect_error(paint_vector(1:3, show_indices = "cell"), "should be one of")
})

# ---------------------------------------------------------------------------
# show_indices takes a VECTOR on a grid
#
# The lanes of a matrix or a data frame are independent, so `c("row", "column")`
# has to mean BOTH -- it is what the package's own README asks for. A rewrite once
# replaced the `any(show_indices %in% ...)` test with `match.arg()`, which is
# length-one by construction, and the README stopped building. No test passed a
# vector, so nothing caught it. These do.
# ---------------------------------------------------------------------------

# The drawn index lanes, for either backend. `paint_*()` invisibly returns the
# resolved table; `gpaint_*()` hides the same table inside its grob.
index_lanes <- function(x) {
  cells <- if (inherits(x, "ggplot")) x$layers[[1L]]$geom_params$grob$cells else x$cells
  sort(intersect(unique(cells$kind), c("cellindex", "rowlabel", "collabel")))
}

test_that("show_indices = c('row', 'column') draws both lanes and no cell indices", {
  m <- matrix(1:15, nrow = 3)

  # The README's call. It must not error, and it must draw exactly two lanes.
  expect_no_error(base <- with_null_pdf(paint_matrix(m, show_indices = c("row", "column"))))
  expect_equal(index_lanes(base), c("collabel", "rowlabel"))
  expect_false("cellindex" %in% index_lanes(base))

  expect_no_error(g <- gpaint_matrix(m, show_indices = c("row", "column")))
  expect_equal(index_lanes(g), c("collabel", "rowlabel"))
  expect_false("cellindex" %in% index_lanes(g))
})

test_that("show_indices = 'all' draws all three lanes", {
  m <- matrix(1:15, nrow = 3)
  expect_equal(
    index_lanes(with_null_pdf(paint_matrix(m, show_indices = "all"))),
    c("cellindex", "collabel", "rowlabel")
  )
  expect_equal(
    index_lanes(gpaint_matrix(m, show_indices = "all")),
    c("cellindex", "collabel", "rowlabel")
  )
})

test_that("each single show_indices value draws exactly its own lane", {
  m <- matrix(1:15, nrow = 3)
  expected <- list(
    none = character(0),
    cell = "cellindex",
    row = "rowlabel",
    column = "collabel"
  )
  for (nm in names(expected)) {
    expect_equal(
      index_lanes(with_null_pdf(paint_matrix(m, show_indices = nm))),
      expected[[nm]],
      info = nm
    )
    expect_equal(index_lanes(gpaint_matrix(m, show_indices = nm)), expected[[nm]], info = nm)
  }
})

test_that("show_indices takes a vector on a data frame too", {
  df <- head(iris, 3)

  base <- with_null_pdf(paint_data_frame(df, show_indices = c("row", "column")))
  expect_equal(index_lanes(base), c("collabel", "rowlabel"))

  expect_equal(
    index_lanes(gpaint_data_frame(df, show_indices = c("row", "column"))),
    c("collabel", "rowlabel")
  )
  # Order does not matter, and a repeat is not an error.
  expect_equal(
    index_lanes(with_null_pdf(paint_data_frame(df, show_indices = c("column", "row", "row")))),
    c("collabel", "rowlabel")
  )
})

test_that("'none' alongside another value loses: the drawn lane wins", {
  # Contradictory, so it is documented: any other value overrides "none".
  m <- matrix(1:4, nrow = 2)
  expect_equal(index_lanes(with_null_pdf(paint_matrix(m, show_indices = c("none", "row")))), "rowlabel")
  expect_equal(index_lanes(gpaint_matrix(m, show_indices = c("none", "row"))), "rowlabel")
})

test_that("an unknown show_indices is still an error, even inside a valid vector", {
  m <- matrix(1:4, nrow = 2)
  # A typo is never silently ignored.
  expect_error(paint_matrix(m, show_indices = c("row", "wombat")), "must be one or more of")
  expect_error(gpaint_matrix(m, show_indices = c("row", "wombat")), "must be one or more of")
  expect_error(paint_data_frame(head(iris, 2), show_indices = c("row", "wombat")), "must be one or more of")
  # The message names the allowed values and what actually arrived.
  expect_error(paint_matrix(m, show_indices = "wombat"), "'none', 'cell', 'row', 'column', or 'all'")
  expect_error(paint_matrix(m, show_indices = "wombat"), "'wombat'")
  # An empty vector is not "no indices", it is a mistake.
  expect_error(paint_matrix(m, show_indices = character(0)), "must be one or more of")
  expect_error(paint_matrix(m, show_indices = NA_character_), "must be one or more of")
})

test_that("paint_vector() still takes exactly one show_indices", {
  # A vector has a single index `[i]`: "inside" and "outside" are placements of the
  # same label, so they are mutually exclusive. main match.arg'd this, and it stays
  # match.arg'd -- widening the grid's argument must not widen the vector's.
  expect_error(paint_vector(1:3, show_indices = c("inside", "outside")), "must be of length 1")
  expect_error(gpaint_vector(1:3, show_indices = c("inside", "outside")), "must be of length 1")
  expect_error(paint_vector(1:3, show_indices = c("none", "inside")), "must be of length 1")
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
  # Assert on the stable classic fill tokens, not the active palette's hexes.
  withr::local_options(paintr.palette = "classic")
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
  expect_false("ellipsis" %in% names(formals(paint_data_frame)))
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

  # ... and a data frame's default is 10, not 20.
  res_df <- with_null_pdf(paint_data_frame(iris))
  expect_equal(sum(res_df$cells$kind == "value"), 9L * 5L)
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
# the data frame is per-column, the matrix is one unit
# ---------------------------------------------------------------------------

test_that("a data frame formats each column independently", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  df <- data.frame(small = c(1, 2), huge = c(1e15, 2e15))
  res <- with_null_pdf(paint_data_frame(df))
  vals <- res$cells[res$cells$kind == "value", , drop = FALSE]

  # Two formatting units, so the huge column flips to scientific and the small one
  # does not.
  expect_identical(sort(unique(vals$fmt_group)), c(1L, 2L))
  small <- vals[vals$fmt_group == 1L, ]
  huge <- vals[vals$fmt_group == 2L, ]
  expect_identical(small$sig, c("1", "2"))
  expect_identical(huge$sig, c("1.00e+15", "2.00e+15"))
  # Hard rule: in scientific mode insig is "" -- the exponent is fully significant.
  expect_identical(unique(huge$insig), "")

  # The header and type rows are there, and they are ASCII.
  expect_identical(res$cells$sig[res$cells$kind == "header"], c("small", "huge"))
  expect_identical(res$cells$sig[res$cells$kind == "type"], c("<dbl>", "<dbl>"))
})

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

test_that("show_names/show_types drop their rows", {
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  df <- head(iris, 3)
  bare <- with_null_pdf(paint_data_frame(df, show_names = FALSE, show_types = FALSE))
  expect_false(any(bare$cells$kind %in% c("header", "type")))

  full <- with_null_pdf(paint_data_frame(df))
  expect_true(any(full$cells$kind == "header"))
  expect_true(any(full$cells$kind == "type"))
  # Two extra drawn rows, and nothing else changed.
  expect_equal(full$cells$row[full$cells$kind == "value"][1L], 3L)
  expect_equal(bare$cells$row[bare$cells$kind == "value"][1L], 1L)
})

# ---------------------------------------------------------------------------
# the label lanes align on request, not by inheritance
#
# The two lanes used to take the VALUE alignment of the column beneath them, so a
# name and a type tag wandered with the column's type: right over a numeric
# column, left over a character one, and lined up on nothing at all in a mixed
# frame. They are centred now, and each lane is settable on its own.
#
# The values must not move. `align == "decimal"` is what anchors a numeric
# column's digits, and no label may cost it that -- which is the half of this
# these tests are really guarding.
# ---------------------------------------------------------------------------

# The cell table of either backend.
painted_cells <- function(x) {
  if (inherits(x, "ggplot")) x$layers[[1L]]$geom_params$grob$cells else x$cells
}

mixed_df <- function() {
  data.frame(
    n = c(1.5, 22.25),
    i = 1:2,
    s = c("a", "b"),
    f = factor(c("u", "v")),
    l = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

# What the five columns' values must align as, in both backends, always.
mixed_value_align <- c("decimal", "decimal", "left", "left", "right")

lane_align <- function(cells, kind) cells$align[cells$kind == kind]

value_align <- function(cells) {
  v <- cells[cells$kind == "value", ]
  vapply(1:5, function(jj) unique(v$align[v$j == jj]), character(1))
}

test_that("both label lanes default to centred, in both backends", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  df <- mixed_df()
  painted <- list(
    base = painted_cells(with_null_pdf(paint_data_frame(df))),
    ggplot = painted_cells(gpaint_data_frame(df))
  )

  for (nm in names(painted)) {
    cells <- painted[[nm]]
    # Every column type, both lanes: centred.
    expect_equal(lane_align(cells, "header"), rep("center", 5L), info = nm)
    expect_equal(lane_align(cells, "type"), rep("center", 5L), info = nm)
    # The values keep their own alignment.
    expect_equal(value_align(cells), mixed_value_align, info = nm)
  }
})

test_that("name_align and type_align each set only their own lane, in both backends", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  df <- mixed_df()

  for (a in c("left", "center", "right")) {
    backends <- list(
      base = list(
        name = painted_cells(with_null_pdf(paint_data_frame(df, name_align = a))),
        type = painted_cells(with_null_pdf(paint_data_frame(df, type_align = a)))
      ),
      ggplot = list(
        name = painted_cells(gpaint_data_frame(df, name_align = a)),
        type = painted_cells(gpaint_data_frame(df, type_align = a))
      )
    )

    for (nm in names(backends)) {
      lab <- paste(nm, a)
      by_name <- backends[[nm]]$name
      expect_equal(lane_align(by_name, "header"), rep(a, 5L), info = lab)
      expect_equal(lane_align(by_name, "type"), rep("center", 5L), info = lab)
      expect_equal(value_align(by_name), mixed_value_align, info = lab)

      by_type <- backends[[nm]]$type
      expect_equal(lane_align(by_type, "type"), rep(a, 5L), info = lab)
      expect_equal(lane_align(by_type, "header"), rep("center", 5L), info = lab)
      expect_equal(value_align(by_type), mixed_value_align, info = lab)
    }
  }

  # Both at once, pulling opposite ways.
  both <- painted_cells(
    with_null_pdf(paint_data_frame(df, name_align = "right", type_align = "left"))
  )
  expect_equal(lane_align(both, "header"), rep("right", 5L))
  expect_equal(lane_align(both, "type"), rep("left", 5L))
  expect_equal(value_align(both), mixed_value_align)

  gboth <- painted_cells(gpaint_data_frame(df, name_align = "right", type_align = "left"))
  expect_equal(lane_align(gboth, "header"), rep("right", 5L))
  expect_equal(lane_align(gboth, "type"), rep("left", 5L))
  expect_equal(value_align(gboth), mixed_value_align)
})

test_that("an invalid name_align or type_align errors, in both backends", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  df <- mixed_df()
  expect_error(with_null_pdf(paint_data_frame(df, name_align = "middle")))
  expect_error(with_null_pdf(paint_data_frame(df, type_align = "middle")))
  expect_error(gpaint_data_frame(df, name_align = "middle"))
  expect_error(gpaint_data_frame(df, type_align = "middle"))
})

test_that("an aligned lane can still be switched off", {
  skip_if_not_installed("ggplot2")
  withr_opt <- options(paintr.warn_floor = FALSE)
  on.exit(options(withr_opt), add = TRUE)

  df <- mixed_df()
  # Alignment does not resurrect a lane the caller dropped.
  no_names <- painted_cells(
    with_null_pdf(paint_data_frame(df, show_names = FALSE, name_align = "left"))
  )
  expect_false(any(no_names$kind == "header"))
  expect_equal(lane_align(no_names, "type"), rep("center", 5L))

  no_types <- painted_cells(gpaint_data_frame(df, show_types = FALSE, type_align = "right"))
  expect_false(any(no_types$kind == "type"))
  expect_equal(lane_align(no_types, "header"), rep("center", 5L))
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
# paint_size(): opens no device, reads no device
# ---------------------------------------------------------------------------

test_that("paint_size() works with no device open at all", {
  # The situation it exists for is "my device is too small", so it must not need one.
  expect_equal(length(grDevices::dev.list()), 0L)

  s <- paint_size(matrix(1:400, nrow = 20), show_all = TRUE)
  expect_named(s, c("width", "height"))
  expect_true(all(is.finite(s)))
  expect_true(all(s > 0))

  # ... and it still opened none.
  expect_equal(length(grDevices::dev.list()), 0L)
  # ... and it wrote no Rplots.pdf, which is what dev.new() would have done.
  expect_false(file.exists("Rplots.pdf"))
})

test_that("paint_size() grows with the data and honours min_pt and units", {
  small <- paint_size(matrix(1:4, nrow = 2))
  big <- paint_size(matrix(1:400, nrow = 20), show_all = TRUE)
  expect_lt(small[["width"]], big[["width"]])
  expect_lt(small[["height"]], big[["height"]])

  # A bigger floor needs a bigger canvas. The HEIGHT says so unconditionally: it is
  # the panel's, and the panel is linear in `min_pt`.
  a <- paint_size(matrix(1:100, nrow = 10), show_all = TRUE, min_pt = 5)
  b <- paint_size(matrix(1:100, nrow = 10), show_all = TRUE, min_pt = 10)
  expect_gt(b[["height"]], a[["height"]])

  # The WIDTH is `max(panel, chrome)`, and for a 10x10 of single digits the chrome
  # WINS at both floors -- the subtitle alone is 59 characters, which is wider than
  # ten one-digit cells will ever be at 10pt. So the width does not move here, and
  # it should not: a device narrower than the title it is about to draw is not a
  # smaller answer, it is a wrong one. It was the bug. It never goes DOWN, though...
  expect_gte(b[["width"]], a[["width"]])

  # ... and the moment the panel is the binding constraint, the width tracks the
  # floor again, exactly as the height does.
  wa <- paint_size(matrix(1:400, nrow = 20), show_all = TRUE, min_pt = 5)
  wb <- paint_size(matrix(1:400, nrow = 20), show_all = TRUE, min_pt = 10)
  expect_gt(wb[["width"]], wa[["width"]])

  # A 2x2 of single digits at 5pt genuinely only needs a fraction of an inch of
  # panel. That is the correct answer, not a bug: it is two tiny cells of 5pt text
  # plus the chrome bands. So the unit assertions are about the CONVERSION, not
  # about a magic size.
  cm <- paint_size(matrix(1:4, nrow = 2), units = "cm")
  px96 <- paint_size(matrix(1:4, nrow = 2), units = "px", dpi = 96)
  px192 <- paint_size(matrix(1:4, nrow = 2), units = "px", dpi = 192)

  expect_gt(cm[["width"]], small[["width"]])
  # Pixels are whole pixels, and they scale with dpi (allowing a pixel of rounding).
  expect_equal(px96[["width"]], ceiling(px96[["width"]]))
  expect_equal(px192[["width"]], 2 * px96[["width"]], tolerance = 0.02)
  # ... and pixels are inches times dpi, to within the round-up.
  expect_lt(abs(px96[["width"]] - small[["width"]] * 96), 96 * 0.1)
})

test_that("paint_size() actually clears the floor at the size it recommends", {
  # The promise, verified end to end: draw at the recommended size and the warning
  # does not fire. This is also what keeps `paint_size()`'s reserved chrome honest
  # -- forget a band the painter draws, and the panel is shorter than the size was
  # computed for, so the text lands UNDER the floor and `floored` comes back TRUE.
  m <- matrix(seq_len(600), nrow = 30)
  s <- paint_size(m, show_all = TRUE, max_rows = 30, max_cols = 20)

  grDevices::pdf(NULL, width = s[["width"]], height = s[["height"]])
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_warning(
    res <- paint_matrix(m, show_all = TRUE, max_rows = 30, max_cols = 20)
  )
  expect_false(res$floored)
})

test_that("paint_size() clears the floor for a data frame too", {
  # A data frame draws two extra rows (names, types) and formats per column, so its
  # panel demand is not a matrix's. Same promise, the other structure.
  s <- paint_size(iris, show_all = TRUE)

  grDevices::pdf(NULL, width = s[["width"]], height = s[["height"]])
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_warning(res <- paint_data_frame(iris, show_all = TRUE))
  expect_false(res$floored)
})

# ---------------------------------------------------------------------------
# paint_size(): THE ROUND TRIP
# ---------------------------------------------------------------------------
#
# The whole contract, and it had never been tested end to end: take the size,
# OPEN A DEVICE AT EXACTLY THAT SIZE, and draw. The man page says "Paste it
# straight into a device call", so that is the test.
#
# It did not work. `paint_size(seq_len(30))` recommended 0.4 x 2.4in, and
# `paint_vector(seq_len(30))` on a 0.4in-wide device ERRORED:
#
#     Error in graphics::par(op) :
#       invalid value specified for graphical parameter "plt"
#
# Two independent bugs, and the round trip needed BOTH of them fixed:
#
#   * the width was taken from the PANEL alone, and a vertical vector's panel is
#     one narrow column -- so the recommendation came out narrower than the
#     title it was going to draw, and narrower than the default `par("mar")`;
#   * and `restore_par()` restored `plt` unguarded, so ANY device too small for
#     the caller's own margins threw on the way out -- after drawing correctly.
#
# The vector is the case that was broken, so the vector is first.

test_that("paint_size(): the recommended size DRAWS, for every structure", {
  cases <- list(
    list(nm = "vector",           d = seq_len(30),            p = paint_vector,     all = FALSE),
    list(nm = "vector show_all",  d = seq_len(200),           p = paint_vector,     all = TRUE),
    list(nm = "vector chr",       d = letters,                p = paint_vector,     all = FALSE),
    list(nm = "matrix",           d = matrix(1:600, nrow = 30), p = paint_matrix,   all = FALSE),
    list(nm = "matrix show_all",  d = matrix(1:600, nrow = 30), p = paint_matrix,   all = TRUE),
    list(nm = "data frame",       d = iris,                   p = paint_data_frame, all = FALSE),
    list(nm = "data frame all",   d = iris,                   p = paint_data_frame, all = TRUE),
    # A list has no nrow() and no ncol(), so `dims_subtitle()` would reserve the
    # chrome against the literal string "Dimensions:  rows x  columns" -- a wrong
    # width, and the wrong line. The ragged one is the case that matters: its
    # bounding box is not its size.
    list(nm = "list",             d = list(a = 1:30, b = "x", c = 1:3), p = paint_list, all = FALSE),
    list(nm = "list show_all",    d = list(a = 1:30, b = "x", c = 1:3), p = paint_list, all = TRUE),
    # ARRAY. It was the one structure missing from this loop -- and the sole other
    # `paint_size()` array test (`test-array.R`) only asserted `> 0`, so nothing here
    # ever drew an array at its OWN recommendation and checked the floor. `extra`
    # carries the arguments the loop's `all`-only shape cannot: `show_indices` and
    # a slice count that forces elision. An empty `extra` reproduces the old
    # `all`-only call exactly, so every case above is unaffected.
    list(nm = "array 3-D",        d = array(1:24, c(2, 3, 4)),         p = paint_array, all = FALSE),
    # Cell-indexed: the case the "under-reports height" report doubted specifically
    # -- every cell carries its own `[i, j, k]` label, which is the widest a block's
    # cells get.
    list(
      nm = "array 3-D cell", d = array(1:24, c(2, 3, 4)), p = paint_array, all = FALSE,
      extra = list(show_indices = "cell")
    ),
    list(nm = "array 4-D",         d = array(1:48, c(2, 3, 4, 2)),      p = paint_array, all = FALSE),
    list(
      nm = "array 4-D cell", d = array(1:48, c(2, 3, 4, 2)), p = paint_array, all = FALSE,
      extra = list(show_indices = "cell")
    ),
    # Titanic: a real, fully-`dimnames()`d contingency table, not a synthetic array.
    list(nm = "array Titanic",     d = Titanic,                        p = paint_array, all = FALSE),
    # UCBAdmissions has 6 slices on its Dept axis, past the `max_slices = 4L`
    # default, so this is also the elision path: three blocks drawn, one "..." block
    # standing in for the rest. See the "3 more slices" note in `?paint_array`.
    list(nm = "array elided",      d = UCBAdmissions,                  p = paint_array, all = FALSE),
    # WRAPPED: a taller, narrower shape than the one-line default. `paint_size()`
    # forwards `slices_per_row` to the builder, so the recommendation must account
    # for the wrapped panel -- and drawing at it must still clear the floor.
    list(
      nm = "array wrapped", d = array(seq_len(3 * 4 * 12), c(3, 4, 12)), p = paint_array, all = TRUE,
      extra = list(slices_per_row = 3)
    ),
    # Wrapped AND elided: the flat slice sequence elides once, its "..." block
    # wrapping into the grid with the rest.
    list(
      nm = "array wrapped elided", d = array(1:120, c(2, 2, 30)), p = paint_array, all = FALSE,
      extra = list(slices_per_row = 2)
    )
  )

  for (cs in cases) {
    extra <- cs$extra
    if (is.null(extra)) extra <- list()
    args <- c(list(cs$d), if (cs$all) list(show_all = TRUE), extra)

    s <- do.call(paint_size, args)

    grDevices::pdf(NULL, width = s[["width"]], height = s[["height"]])
    dev <- grDevices::dev.cur()

    # (a) IT DRAWS. No error -- and the error it used to throw came from the
    # `on.exit()` restore, i.e. AFTER the picture, so nothing short of running the
    # painter for real would have caught it.
    res <- NULL
    expect_no_error(
      res <- do.call(cs$p, args),
      message = cs$nm
    )
    # (b) AND IT CLEARS THE FLOOR, which is what the size was computed to do.
    # `min_pt` defaults to 5 in `paint_size()` and in `paint_opts()` alike.
    expect_gte(res$fontsize, 5)
    expect_false(res$floored)

    grDevices::dev.off(dev)
  }
})

test_that("paint_size() recommends a device wider than the chrome it will draw", {
  # The mechanism, isolated. `base_mai()` models the chrome as vertical BANDS and
  # never looks at their width, so a width taken from the panel alone forgets the
  # title and the subtitle entirely -- and for a vertical vector, which is one
  # narrow column, the chrome is ALL of the width.
  s <- paint_size(seq_len(30))

  title <- "Data Object: seq_len(30)"
  subtitle <- vector_subtitle(seq_len(30))
  m <- measure_mono("mono")

  # `base_mai()`'s side margins, which the recommendation adds to the panel.
  sides <- base_mai(title, subtitle)[[2L]] + base_mai(title, subtitle)[[4L]]

  expect_gte(s[["width"]], m$w(title, 12)[[1L]] + sides)
  expect_gte(s[["width"]], m$w(subtitle, 9)[[1L]] + sides)
  # Non-vacuous: this is the assertion that fails on the old arithmetic, which
  # returned 0.4in -- narrower than the 24-character title at 12pt (2.4in).
  expect_gt(m$w(title, 12)[[1L]], 0.4)
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
    # A data frame is a grid, so it gets the grid line -- rows, columns, class.
    # Three painters, one contract: a default that two of them drew and the third
    # did not would be the same drift as a subtitle only one backend draws.
    expect_equal(
      paint_data_frame(head(iris, 3))$graph_subtitle,
      "Dimensions: 3 rows x 5 columns | Data Type: data.frame"
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
    # ELEMENTS. The subtitle one line above already says so, and the note used to
    # contradict it: a vector drawn down the page was told it had "31 more rows",
    # and laid across it, "more columns". It has neither.
    expect_match(rv$note, "^# 31 more elements$")

    rd <- paint_data_frame(iris)
    expect_equal(
      rd$graph_subtitle,
      "Dimensions: 150 rows x 5 columns | Data Type: data.frame"
    )
    expect_match(rd$note, "more rows")
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
  expect_equal(
    gpaint_data_frame(head(iris, 3))$labels$subtitle,
    "Dimensions: 3 rows x 5 columns | Data Type: data.frame"
  )
  expect_null(gpaint_matrix(m, graph_subtitle = NA)$labels$subtitle)
  expect_equal(gpaint_matrix(m, graph_subtitle = "mine")$labels$subtitle, "mine")
  expect_null(gpaint_data_frame(head(iris, 3), graph_subtitle = NA)$labels$subtitle)
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

  paint_data_frame(head(iris, 3))
  expect_equal(graphics::par(no.readonly = TRUE), before)

  paint_list(list(a = 1:4, b = "x"))
  expect_equal(graphics::par(no.readonly = TRUE), before)
})

# ---------------------------------------------------------------------------
# names and dimnames, through both backends
#
# The picture must not be less informative than `print()`.
# ---------------------------------------------------------------------------

lane_sig <- function(x, kind) {
  cells <- painted_cells(x)
  cells$sig[cells$kind == kind]
}

test_that("both backends draw a vector's names", {
  skip_if_not_installed("ggplot2")
  v <- c(alpha = 1, beta = 2, gamma = 3)

  # The name lane is the accessor: `v["alpha"]`.
  base <- with_null_pdf(paint_vector(v))
  expect_equal(lane_sig(base, "rowlabel"), c('["alpha"]', '["beta"]', '["gamma"]'))

  g <- with_null_pdf(gpaint_vector(v))
  expect_equal(lane_sig(g, "rowlabel"), c('["alpha"]', '["beta"]', '["gamma"]'))

  # Horizontal puts them over the cells instead.
  h <- with_null_pdf(paint_vector(v, layout = "horizontal"))
  expect_equal(lane_sig(h, "collabel"), c('["alpha"]', '["beta"]', '["gamma"]'))

  # And they compose with an in-cell index, which stays POSITIONAL: the name is
  # already the whole accessor in the gutter, so the in-cell lane adds the position.
  both <- with_null_pdf(paint_vector(v, show_indices = "inside"))
  expect_equal(lane_sig(both, "rowlabel"), c('["alpha"]', '["beta"]', '["gamma"]'))
  expect_equal(lane_sig(both, "cellindex"), c("[1]", "[2]", "[3]"))

  off <- with_null_pdf(paint_vector(v, show_names = FALSE))
  expect_equal(length(lane_sig(off, "rowlabel")), 0L)
  goff <- with_null_pdf(gpaint_vector(v, show_names = FALSE))
  expect_equal(length(lane_sig(goff, "rowlabel")), 0L)
})

test_that("both backends draw a matrix's dimnames", {
  skip_if_not_installed("ggplot2")
  m <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))

  base <- with_null_pdf(paint_matrix(m))
  expect_equal(lane_sig(base, "rowlabel"), c('["r1", ]', '["r2", ]'))
  expect_equal(lane_sig(base, "collabel"), c('[, "c1"]', '[, "c2"]', '[, "c3"]'))

  g <- with_null_pdf(gpaint_matrix(m))
  expect_equal(lane_sig(g, "rowlabel"), c('["r1", ]', '["r2", ]'))
  expect_equal(lane_sig(g, "collabel"), c('[, "c1"]', '[, "c2"]', '[, "c3"]'))

  # The doc example: the name above the column, the accessor under the value.
  both <- with_null_pdf(paint_matrix(m, show_indices = "cell"))
  expect_equal(lane_sig(both, "collabel"), c('[, "c1"]', '[, "c2"]', '[, "c3"]'))
  expect_true('["r2", "c3"]' %in% lane_sig(both, "cellindex"))

  # An index lane the user asked for wins over the names on that axis.
  idx <- with_null_pdf(paint_matrix(m, show_indices = "row"))
  expect_equal(lane_sig(idx, "rowlabel"), c("[1, ]", "[2, ]"))
  expect_equal(lane_sig(idx, "collabel"), c('[, "c1"]', '[, "c2"]', '[, "c3"]'))

  none <- with_null_pdf(paint_matrix(m, show_dimnames = "none"))
  expect_equal(length(lane_sig(none, "rowlabel")), 0L)
  expect_equal(length(lane_sig(none, "collabel")), 0L)

  gnone <- with_null_pdf(gpaint_matrix(m, show_dimnames = c("none")))
  expect_equal(length(lane_sig(gnone, "collabel")), 0L)

  expect_error(paint_matrix(m, show_dimnames = "banana"), "must be one or more of")
  expect_error(gpaint_matrix(m, show_dimnames = "banana"), "must be one or more of")
})

test_that("both backends draw a data frame's row names, when they are real", {
  skip_if_not_installed("ggplot2")
  df <- head(mtcars[, 1:3], 3)
  # The gutter keeps `max_chars`, and "Mazda RX4 Wag" is one character over it. It
  # is drawn as the accessor: `df["Mazda RX4", ]`.
  rn <- c('["Mazda RX4", ]', '["Mazda RX4...", ]', '["Datsun 710", ]')

  base <- with_null_pdf(paint_data_frame(df))
  expect_equal(lane_sig(base, "rowlabel"), rn)
  # And never the column names twice.
  expect_equal(length(lane_sig(base, "collabel")), 0L)
  expect_equal(lane_sig(base, "header"), names(df))

  g <- with_null_pdf(gpaint_data_frame(df))
  expect_equal(lane_sig(g, "rowlabel"), rn)

  # iris has no row names of its own: a gutter of 1, 2, 3 is noise.
  plain <- with_null_pdf(paint_data_frame(head(iris, 3)))
  expect_equal(length(lane_sig(plain, "rowlabel")), 0L)

  off <- with_null_pdf(paint_df(df, show_rownames = FALSE))
  expect_equal(length(lane_sig(off, "rowlabel")), 0L)
})
