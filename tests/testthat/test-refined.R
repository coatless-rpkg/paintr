# The refined header structure of a styled (non-classic) palette.
#
# Three things arrive together when `styled = TRUE`, and only for a structure that
# draws a block outline AND a column-name/type header -- a data frame or a
# rectangular, un-gapped list:
#
#   1. the column-name cells draw in BOLD;
#   2. the block outline rises to enclose the header (names + type) rows, so the
#      card holds header and body as ONE rectangle;
#   3. a single `"headerband"` cell tints that header region.
#
# A matrix, vector or array has no header, so it gets none of this; a ragged or
# gapped list draws no outline, so it gets no band. And `styled = FALSE` (classic)
# takes none of it at all -- the cell table is byte-identical to before this layer,
# which the last test pins against baselines captured before the palette existed.

# A null pdf device, torn down at the end of the calling test: both production
# measures need an open device to measure against.
local_null_pdf <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(dev), envir = env)
  invisible(dev)
}

# ---------------------------------------------------------------------------
# the header band
# ---------------------------------------------------------------------------

test_that("a styled data frame emits exactly one header band over the header rows", {
  cells <- paint_cells(head(mtcars, 4), styled = TRUE)

  band <- cells[cells$kind == "headerband", , drop = FALSE]
  expect_equal(nrow(band), 1L)
  expect_identical(band$fill, "headerband")

  # It spans the header (names + type) rows -- the rows ABOVE the first value row.
  header_rows <- cells$row[cells$kind == "header"]
  type_rows <- cells$row[cells$kind == "type"]
  first_value_row <- min(cells$row[cells$kind == "value"])
  expect_equal(band$row, min(c(header_rows, type_rows)))
  expect_equal(band$row_end, first_value_row - 1L)

  # Its column span is the extended outline's: the row-label gutter (column 1)
  # stays OUTSIDE, so the band runs from the first data column to the last.
  outline <- cells[cells$kind == "outline", , drop = FALSE]
  expect_equal(band$col, outline$col)
  expect_equal(band$col_end, outline$col_end)
  expect_equal(band$col_end, max(cells$col))

  # It carries no text, so nothing is drawn for it as ink.
  expect_identical(band$sig, "")
  expect_identical(band$insig, "")

  # Under mint the tint resolves to the palette's header colour.
  resolved <- apply_palette(band, resolve_palette("mint"))
  expect_identical(resolved$fill, "#F4F7F8")
})

test_that("the header column names are bold only when styled", {
  styled <- paint_cells(head(mtcars, 4), styled = TRUE)
  classic <- paint_cells(head(mtcars, 4), styled = FALSE)

  expect_true(all(styled$fontface[styled$kind == "header"] == "bold"))
  expect_true(all(classic$fontface[classic$kind == "header"] == "plain"))
  # Nothing else goes bold: the type tags and the values stay plain either way.
  expect_true(all(styled$fontface[styled$kind != "header"] == "plain"))
})

test_that("the outline rises to enclose the header when styled", {
  styled <- paint_cells(head(mtcars, 4), styled = TRUE)
  classic <- paint_cells(head(mtcars, 4), styled = FALSE)

  top_styled <- styled$row[styled$kind == "outline"]
  top_classic <- classic$row[classic$kind == "outline"]
  expect_lt(top_styled, top_classic)

  # The styled outline's top is the topmost header/type row; the classic outline's
  # top is the first value row, exactly as before.
  expect_equal(top_styled, min(styled$row[styled$kind %in% c("header", "type")]))
  expect_equal(top_classic, min(classic$row[classic$kind == "value"]))
  # The card bottom is untouched: both run to the last drawn row.
  expect_equal(
    styled$row_end[styled$kind == "outline"],
    classic$row_end[classic$kind == "outline"]
  )
})

# ---------------------------------------------------------------------------
# only the structures that draw a header card get one
# ---------------------------------------------------------------------------

test_that("a rectangular list gets a band, a ragged or gapped one does not", {
  rect <- paint_cells(list(a = 1:3, b = 4:6), styled = TRUE)
  ragged <- paint_cells(list(a = 1:3, b = 1), styled = TRUE)
  gapped <- paint_cells(list(a = 1:3, b = 4:6), styled = TRUE, gap = 0.5)

  expect_equal(sum(rect$kind == "headerband"), 1L)
  expect_equal(sum(ragged$kind == "headerband"), 0L)
  expect_equal(sum(gapped$kind == "headerband"), 0L)

  # The band tracks the outline: the rectangular list draws one, the other two
  # draw none, so a band without an outline never happens.
  expect_equal(sum(rect$kind == "outline"), 1L)
  expect_equal(sum(ragged$kind == "outline"), 0L)
  expect_equal(sum(gapped$kind == "outline"), 0L)
})

test_that("a matrix and a vector get no band and no bold even when styled", {
  m <- paint_cells(matrix(1:15, nrow = 3), styled = TRUE, show_indices = "cell")
  v <- paint_cells(c(-3, 5, NA, Inf, 2, 1), styled = TRUE)

  expect_equal(sum(m$kind == "headerband"), 0L)
  expect_equal(sum(v$kind == "headerband"), 0L)
  expect_false(any(m$fontface == "bold"))
  expect_false(any(v$fontface == "bold"))
})

# ---------------------------------------------------------------------------
# classic is byte-identical to before this layer existed
# ---------------------------------------------------------------------------

test_that("styled = FALSE still equals the pre-palette baseline cell tables", {
  # The baselines are raw `paint_cells()` outputs captured BEFORE the palette
  # layer -- the geometry-and-token columns only, since `fontface` is new. Every
  # one must still come back byte-for-byte from `styled = FALSE`.
  cols <- c(
    "row", "col", "row_end", "col_end", "kind", "ink", "fill", "border",
    "lwd", "align", "size_rel", "sig", "insig"
  )
  same <- function(cells, tag) {
    base <- readRDS(test_path("fixtures", "classic-baseline", paste0("base_", tag, ".rds")))
    got <- as.data.frame(cells)[, cols, drop = FALSE]
    expect_true(isTRUE(all.equal(got, base, check.attributes = FALSE)), info = tag)
  }

  same(paint_cells(c(-3, 5, NA, Inf, 2, 1), styled = FALSE, ellipsis = "..."), "vector")
  same(paint_cells(matrix(1:15, nrow = 3), styled = FALSE, ellipsis = "...", show_indices = "cell"), "matrix")
  same(paint_cells(head(mtcars, 4), styled = FALSE, ellipsis = "..."), "dframe")
  same(paint_cells(list(id = 1:3, tags = c("a", "b"), ok = c(TRUE, FALSE, NA)), styled = FALSE, ellipsis = "..."), "list")
  same(paint_cells(array(1:24, dim = c(2, 3, 4)), styled = FALSE, ellipsis = "...", show_indices = "cell"), "array")
})

# ---------------------------------------------------------------------------
# fontface survives the resolve both backends share
# ---------------------------------------------------------------------------

test_that("the fontface column survives paint_resolve identically on either backend", {
  local_null_pdf()
  df <- head(mtcars, 4)
  cells <- paint_cells(df, styled = TRUE)
  cw <- column_widths(cells)
  nr <- attr(cells, "n_row")
  opts <- paint_opts(palette = "mint")

  base <- paint_resolve(cells, cw, nr, panel_fake(7, 5), measure_base("mono"), opts)$cells
  grid <- paint_resolve(cells, cw, nr, panel_fake(7, 5), measure_grid("mono"), opts)$cells

  # The column is carried through unchanged, and the two backends agree on it, on
  # the ink, and on the fill -- so the bold names and the tinted band are one
  # picture, whichever renderer draws them.
  expect_identical(base$fontface, grid$fontface)
  expect_true(any(base$fontface == "bold"))
  expect_identical(base$fontface[base$kind == "header"], rep("bold", sum(base$kind == "header")))
  expect_identical(base$ink, grid$ink)
  expect_identical(base$fill, grid$fill)
})
